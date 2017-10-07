{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Server.WebSocket
  ( websocketMiddleware
  , websocketServer
  , websocketCacheInvalidationService
  , websocketGCService
  , mkWSSessionId
  ) where
#include "import_backend.hs"

import qualified "cryptonite" Crypto.Hash as CRT
import qualified Crypto.Random.Entropy as CRT
import           Network.Wai.Handler.WebSockets
import           Network.Wai (Middleware)
import           Network.WebSockets
import           Network.WebSockets.Connection
import           System.Timeout (timeout)

import Refine.Backend.App
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Server.Types
import Refine.Common.Types


-- | Accept HTTP connections; negotiate 'WSSessionId'; create new or find existing 'WSSession';
-- start 'websocketConnectionLoop'.
websocketMiddleware :: Config -> LogChan -> RunApp DB -> MVar WSSessionMap -> Middleware
websocketMiddleware cfg logchan rapp sessmap =
  websocketsOr options (websocketServerApp cfg logchan rapp sessmap)
  where
    options :: ConnectionOptions
    options = defaultConnectionOptions

-- | this is for debugging only it opens a ws listener directly (without wai) on localhost:<port> (host
-- cannot be configured).
websocketServer :: Config -> LogChan -> RunApp DB -> MVar WSSessionMap -> IO (Async ())
websocketServer cfg logchan rapp sessmap = do
  as <- async $ runServerWith host port options (websocketServerApp cfg logchan rapp sessmap)
  -- before we return, see if there is a start-time exception, and if so, throw it.
  threadDelay $ 30 * 1000
  poll as >>= \case
    Nothing        -> pure as
    Just (Left e)  -> throwIO e
    Just (Right v) -> throwIO . ErrorCall $ "websocketServer: returned immediately and unexpectedly with " <> show v
  where
    options :: ConnectionOptions = defaultConnectionOptions
    host    :: String            = "127.0.0.1"
    port    :: Int               = cfg ^. cfgWarpSettings . warpSettingsPort

websocketServerApp :: Config -> LogChan -> RunApp DB -> MVar WSSessionMap -> ServerApp
websocketServerApp cfg logchan rapp sessmap pendingconnection = do
  conn <- acceptRequest pendingconnection
  pingLoop rapp conn
  withWSSession cfg conn logchan sessmap
    $ websocketConnectionLoop conn logchan rapp

pingLoop :: (Database db, HasCallStack) => RunApp db -> Connection -> IO ()
pingLoop rapp conn = do
  ([], Right interval) <- runAppTC rapp . asks . view $ appConfig . cfgWSPingPeriod
  forkPingThread conn $ timespanSecs interval


-- | receive connection message; create / update 'WSSession' in session map; flush 'ToClient' chan;
-- send 'WSMsgAccept'; pass session to loop action.  finally (after loop action returns or throws
-- anything), update 'WSSession' again and return.
--
-- the actual loop is run in an 'async' so it can be cancelled from elsewhere: if the connection
-- dies, but the loop takes a bit longer to finish than the client to re-connect, then the old
-- session loop is still running while the new is supposed to be started.  in this situation, the
-- 'Async' is collected from the 'WSSession' and cancelled.
withWSSession :: Config -> Connection -> LogChan -> MVar WSSessionMap -> (WSSessionId -> WSSession -> IO ()) -> IO ()
withWSSession cfg conn logchan sessmap loop = do
  (clientId, sess) <- handshakeStart
  as <- async $ loop clientId sess `finally` handshakeEnd sess
  putMVar (sess ^. wssLastSeen) (Right as)  -- putMVar
  wait as
  where
    handshakeStart :: IO (WSSessionId, WSSession)
    handshakeStart = receiveMessage Nothing conn logchan >>= \case
      HandshakeToServerSyn -> mkFresh
      HandshakeToServerSynWith clientId -> maybe mkFresh (reloadOld clientId) . Map.lookup clientId =<< readMVar sessmap
      where
        mkFresh :: IO (WSSessionId, WSSession)
        mkFresh = do
          sess <- WSSession
            <$> newMVar (initialAppState cfg)
            <*> newTChanIO
            <*> newEmptyMVar  -- newEmptyMVar
          clientId <- mkWSSessionId
          modifyMVar_ sessmap $ pure . Map.insert clientId sess
          sendMessage (Just clientId) conn logchan (HandshakeToClientAcceptNew clientId)
          pure (clientId, sess)

        reloadOld :: WSSessionId -> WSSession -> IO (WSSessionId, WSSession)
        reloadOld clientId sess = do
          takeMVar (sess ^. wssLastSeen) >>= \case  -- takeMVar
            Right as -> cancel as
            Left _   -> pure ()

          msgs <- let flush c = atomically (tryReadTChan c) >>= \case
                        Nothing -> pure []
                        Just e  -> (e:) <$> flush c
                  in flush (sess ^. wssToClientQ)
          sendMessage (Just clientId) conn logchan (HandshakeToClientAccept clientId msgs)
          pure (clientId, sess)

    handshakeEnd :: WSSession -> IO ()
    handshakeEnd sess = getCurrentTimestamp >>= \ts -> modifyMVar_ (sess ^. wssLastSeen) (const . pure $ Left ts)

mkWSSessionId :: IO WSSessionId
mkWSSessionId = do
  rnd  <- CRT.getEntropy 512
  time <- cs . show <$> getCurrentTimestamp
  pure . WSSessionId . cs . show . CRT.hash @SBS @CRT.SHA3_512 $ rnd <> time

-- | Read 'ToServer' messages from ws connection and pass them to 'runToServer'.  Before entering
-- this loop, trigger some initial cache feeding so the client does not have to do it.
--
-- If the client does not send any messages for more than 30 minutes, terminate the connection.
--
-- This function only receives on web sockets, only sends on the 'ToClient' chan, and locks the
-- 'AppState'.  (The threads created here compete with the thread created in
-- 'websocktCacheInvalidationService' for the 'AppState'.)
--
-- If it weren't for logging, hte 'WSSessionId' wouldn't be required here.
websocketConnectionLoop :: Connection -> LogChan -> RunApp DB -> WSSessionId -> WSSession -> IO ()
websocketConnectionLoop conn logchan rapp clientId (WSSession appState toClientQ _) =
  initially >> sendLoop >>= recvLoop
  where
    initially :: IO ()
    initially = runApp' rapp toClientQ $ fetchCache (Set.fromList [CacheKeyGroupIds, CacheKeyUserIds])

    -- read from 'ToCLient' queue and write to 'Connection'.
    sendLoop :: IO (Async ())
    sendLoop = asyncChanPipe toClientQ (sendMessage (Just clientId) conn logchan)

    -- read from 'Connection' and call 'runToServer' (which may in turn feed this session's
    -- 'ToClient' queue or that of others).
    recvLoop :: Async () -> IO ()
    recvLoop sending = do
      pollmsg >>= \case
        Left errmsg -> do
          cancel sending
          logWSMessage (Just clientId) logchan $ "closing connection (" <> errmsg <> ")"
        Right msg -> do
          result <- runExceptT . runApp rapp toClientQ $ do
            put =<< liftIO (takeMVar appState)
            runToServer msg
              `finally` (get >>= liftIO . putMVar appState)
          case result of
            Right () -> pure ()
            Left apiError -> do
              logWSMessage (Just clientId) logchan $ show apiError
              atomically $ writeTChan toClientQ (TCError apiError)
          recvLoop sending

    -- handle timeouts and exceptions thrown by 'receiveMessage'.
    pollmsg :: IO (Either String ToServer)
    pollmsg = join <$> tmo (rcv `catch` hdl)
      where
        rcv :: IO (Either a ToServer)
        rcv = Right <$> receiveMessage (Just clientId) conn logchan

        hdl :: SomeException -> IO (Either String a)
        hdl = pure . Left . ("threw " <>) . show

        tmo :: IO a -> IO (Either String a)
        tmo action = timeout (timespanUs timespan) action >>= \case
          Just a  -> pure . Right $ a
          Nothing -> pure . Left  $ "inactive for " <> show timespan
          where
            timespan = TimespanMins 30


-- | Read invalidated 'CacheKey's from central channel; call 'sendInvalidateCaches' on all sessions
-- in 'WSSessionMap'.
--
-- TUNING: collect messages from queue for a few miliseconds, then concatenate all keys received,
-- then send a round of invalidation messages to clients.  not sure how much that will bring in
-- better UX, but it's easy to implement.
websocketCacheInvalidationService :: MVar WSSessionMap -> TChan (Set CacheKey) -> IO (Async ())
websocketCacheInvalidationService sessmap cacheKeyQ = async . forever $
  atomically (readTChan cacheKeyQ) >>= \ckeyset -> do
    sessions <- Map.elems <$> readMVar sessmap
    sendInvalidateCaches ckeyset `mapM_` sessions

-- | Check if client connected to session holds any invalidated keys in its cache.  If so, remove
-- them from the stored 'AppState' and notify the client.
--
-- FIXME: the invalidation messages are sent to the client after the 'AppState' has already been
-- updated (with the MVar lock removed and everything).  not sure if there are unpleasant race
-- conditions hidden here somewhere?
sendInvalidateCaches :: Set CacheKey -> WSSession -> IO ()
sendInvalidateCaches invalidated sess = do
  let upd :: AppState -> (AppState, [CacheKey])
      upd st =
        let presentBefore = st ^. appCacheState
        in case Set.partition (`Set.member` invalidated) presentBefore of
          (removed, _) | Set.null removed -> do
            (st, [])
          (removed, presentAfter) -> do
            (st & appCacheState .~ presentAfter, Set.toList removed)

  result <- modifyMVar (sess ^. wssAppState) (pure . upd)
  case result of
    keys@(_:_) -> atomically $ writeTChan (sess ^. wssToClientQ) (TCInvalidateKeys keys)
    [] -> pure ()


-- | sweep through 'WSSessionMap' and remove all 'WSSessionIds' that have a) invalid sessions, *AND*
-- b) have not been run by 'websocketConnectionLoop' in the last 30 minutes.  (the latter is needed
-- to account for sessions without authenticated user.)
--
-- TODO: also check session validity!  if session is valid, do not gc!
websocketGCService :: Config -> MVar WSSessionMap -> IO (Async ())
websocketGCService _cfg sessmap = async . forever $ do
  threadDelay . timespanUs $ TimespanMins 37
  now <- getCurrentTimestamp

  let stillgood :: WSSession -> IO Bool
      stillgood ((^. wssLastSeen) -> lastused) = do
        tryReadMVar lastused <&> \case
          Nothing -> True  -- (about to get updated)
          Just (Right _) -> True  -- (conn listener still running)
          Just (Left backthen) -> now `diffTimestamps` backthen < TimespanMins 30

  modifyMVar_ sessmap $ filterMapM stillgood

-- | TUNING: there must be a 'filterM' variant out there somewhere that already does that?
filterMapM :: (Ord a) => (b -> IO Bool) -> Map a b -> IO (Map a b)
filterMapM f = fmap Map.fromList . filterM (f . snd) . Map.toList


-- * helpers

sendMessage :: (Show toclient, ToJSON toclient)
            => Maybe WSSessionId -> Connection -> LogChan -> toclient -> IO ()
sendMessage mclientId conn logchan msg = do
  logWSMessage mclientId logchan msg
  liftIO $ sendTextData conn (cs $ encode msg :: ST)

receiveMessage :: forall toserver. (Show toserver, FromJSON toserver, Typeable toserver)
               => Maybe WSSessionId -> Connection -> LogChan -> IO toserver
receiveMessage mclientId conn logchan = do
  emsg <- liftIO $ eitherDecode <$> receiveData conn
  case emsg of
    Left err -> giveup logchan $ "could not parse client message: "
                       <> show (mclientId, typeOf (undefined :: toserver), err)
    Right msg -> logWSMessage mclientId logchan msg >> pure msg

logWSMessage :: (MonadIO m, Show a) => Maybe WSSessionId -> LogChan -> a -> m ()
logWSMessage mclientId logchan msg = do
  let tagged = cs ("[" <> maybe "no session" (ST.take 10 . unWSSessionId) mclientId <> "] ") <> show msg
  liftIO $ appLog LogDebug tagged `runReaderT` logchan

-- | FIXME: can we do better than this?  more error type info, handle some errors more gracefully?
giveup :: MonadIO m => LogChan -> String -> m a
giveup logchan msg = do
  liftIO $ appLog LogWarning msg `runReaderT` logchan
  liftIO . throwIO $ ErrorCall msg
