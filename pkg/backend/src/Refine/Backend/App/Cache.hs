{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | (This module is called "Cache" for historical reasons.  It's really more of a "WebSocket"
-- protocol module, perhaps we should rename it.)
module Refine.Backend.App.Cache  -- FIXME: rename to WebSocket.  this is the only place where
  ( startWebSocketServer         -- communication happens, it's not just about caching.
  , resetWebSocketMVar

    -- * exported for testing only
  , mkWSSessionId
  ) where
#include "import_backend.hs"

import           Control.Concurrent
import qualified "cryptonite" Crypto.Hash as CRT
import qualified Crypto.Random.Entropy as CRT
import           Network.Wai.Handler.WebSockets
import           Network.Wai (Middleware)
import           Network.WebSockets
import           System.IO.Unsafe
import           System.Timeout

import Refine.Common.Types
import Refine.Common.Rest (ApiError(..))
import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import {-# SOURCE #-} Refine.Backend.App.Translation as App (getTranslations)
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger


-- TODO: we need an extra timeout mechanism here: every session that has not been touched in 30
-- minutes should be verified.  if no user is logged in or the login session is invalid, call
-- 'celeteWsConn'.  (this is garbage collection, not correctness or security; things should work
-- without if we have infinite memory.)


type WebSocketMVar = MVar (Map WSSessionId WebSocketSession)
type WebSocketSession = ((Connection, MVar AppState), Set CacheKey)

-- | This is the place where all the session's 'AppState's are kept between websocket
-- connections.
{-# NOINLINE webSocketMVar #-}
webSocketMVar :: WebSocketMVar
webSocketMVar = unsafePerformIO $ newMVar mempty

-- | Clear the web socket server state and destroy all sessions.
--
-- This used to be a work-around for deadlocks in some 'WebSocketSpec' tests.  Most likely the
-- deadlocks were caused by uncaught exceptions in 'AppM', with the consequence that 'webSocketMVar'
-- was left empty between two test cases (and their server runs).  This shouldn't happen any more,
-- but it's still good to make sure that the web socket state is empty when a server starts.  NOTE:
-- this is mostly an issue in tests; in production, we only ever start one server and leave that
-- running forever.
resetWebSocketMVar :: IO ()
resetWebSocketMVar = tryTakeMVar webSocketMVar >> putMVar webSocketMVar mempty

mkWSSessionId :: IO WSSessionId
mkWSSessionId = do
  rnd  <- CRT.getEntropy 512
  time <- cs . show <$> getCurrentTimestamp
  pure . WSSessionId . cs . show . CRT.hash @SBS @CRT.SHA3_512 $ rnd <> time

openWsConn :: Config -> Connection -> IO WSSessionId
openWsConn cfg conn = do
  appState <- newMVar $ initialAppState cfg
  modifyMVar webSocketMVar $ \cmap -> do
    i <- mkWSSessionId
    let cmap' = Map.insert i ((conn, appState), mempty) cmap
    pure (cmap', i)

-- | see 'sendClose' docs.  FUTUREWORK: the websockets protocol seems overly complicated here; isn't
-- letting the connection die more efficient for both cooperating clients and the server?
closeWsConn :: Connection -> Logger -> Maybe WSSessionId -> Maybe String -> IO ()
closeWsConn conn logger mclientId mreason = do
  (appLog LogDebug `mapM_` mreason) `runReaderT` logger
  sendClose @ST conn "closing web socket connection."
  let pull = do
        forever . void $ receiveDataMessage conn
      tout act = do
        maybe (throwIO $ ErrorCall "timeout") pure
          =<< timeout (timespanUs $ TimespanSecs 70) act
      hndl (SomeException status) = do
        appLog LogDebug ("closeWsConn: " <> show (mclientId, status))
          `runReaderT` logger
  void . forkIO $ tout pull `catch` hndl

deleteWsConn :: WSSessionId -> IO ()
deleteWsConn n = do
  modifyMVar webSocketMVar $ \cmap ->do
    pure (Map.delete n cmap, ())


class MonadWS m where
  sendMessage    :: Maybe WSSessionId -> Connection -> ToClient -> m ()
  receiveMessage :: Maybe WSSessionId -> Connection -> m ToServer
  modifyCache    :: WSSessionId -> (Set CacheKey -> Set CacheKey) -> m ()

instance (Monad m, MonadLog m, MonadIO m) => MonadWS m where
  sendMessage mclientId conn msg = do
    logWSMessage mclientId msg
    liftIO $ sendTextData conn (cs $ encode msg :: ST)

  receiveMessage mclientId conn = do
    msg <- liftIO $ receiveData conn <&> fromMaybe (error "websockets json decoding error") . decode
    logWSMessage mclientId msg
    pure msg

  modifyCache sid f = liftIO . modifyMVar webSocketMVar $ \m -> pure (Map.adjust (second f) sid m, ())

logWSMessage :: (Monad m, MonadLog m, Show a) => Maybe WSSessionId -> a -> m ()
logWSMessage mclientId msg = appLog LogDebug $
  cs ("[" <> maybe "no session" (ST.take 10 . unWSSessionId) mclientId <> "] ") <>
  show msg


getData :: CacheKey -> App ServerCache
getData = \case
  CacheKeyVDoc i       -> App.getVDoc i       <&> \val -> mempty & scVDocs       .~ Map.singleton i val
  CacheKeyEdit i       -> App.getEdit i       <&> \val -> mempty & scEdits       .~ Map.singleton i val
  CacheKeyDiscussion i -> App.getDiscussion i <&> \val -> mempty & scDiscussions .~ Map.singleton i val
  CacheKeyUser i       -> App.getUser i       <&> \val -> mempty & scUsers       .~ Map.singleton i val
  CacheKeyGroup i      -> App.getGroup i      <&> \val -> mempty & scGroups      .~ Map.singleton i val
  CacheKeyGroupIds     -> App.getGroups       <&> \val -> mempty & scGroupIds    .~ Just (Set.fromList $ (^. groupID) <$> val)
  CacheKeyUserIds      -> App.getUsers        <&> \val -> mempty & scUserIds     .~ Just val


instance Database db => MonadCache (AppM db) where
    invalidateCaches keys = do
        cmap <- liftIO $ takeMVar webSocketMVar
        let cmap' = second (Set.\\ keys) <$> cmap
            cls = [(n, conn, d) | (n, ((conn, _), s)) <- Map.assocs cmap, let d = Set.intersection keys s, not $ Set.null d]
        forM_ cls $ \(n, conn, d) -> do
            sendMessage (Just n) conn . TCInvalidateKeys $ Set.toList d
        liftIO $ putMVar webSocketMVar cmap'

-- | FIXME: delete disconnected clients' data
startWebSocketServer :: Config -> (forall a. AppM DB a -> IO (Either ApiError a)) -> Middleware
startWebSocketServer cfg toIO = websocketsOr options server
  where
    options :: ConnectionOptions
    options = defaultConnectionOptions

    server :: ServerApp
    server pendingconnection = do
      conn <- acceptRequest pendingconnection
      pingLoop toIO conn
      Right logger <- toIO $ (asks (view appLogger) :: AppM db Logger)
      (do
        handshake Nothing cfg conn logger >>= \case
          Right clientId -> do
            let loop = do
                  b <- (cmdLoopWrapRound toIO cfg conn logger clientId >> pure True)
                    `catch` handleAllErrors cfg conn logger clientId
                  when b loop
            loop
          Left err -> giveUp conn logger Nothing "initial handshake" err)
        `catch` giveUp @SomeException conn logger Nothing "exception during initial handshake"

handleAllErrors :: Config -> Connection -> Logger -> WSSessionId -> SomeException -> IO Bool
handleAllErrors cfg conn logger clientId err = (handle >> pure True)
  `catch` \(SomeException err') -> giveUp conn logger (Just clientId) "unknown error" err' >> pure False
  where
    handle = do
      let servermsg = "handleAllErrors: " <> show err
          clientmsg = "handleAllErrors: unexpected exception in AppM"
      logWSMessage (Just clientId) servermsg `runReaderT` logger
      sendMessage Nothing conn (TCError $ ApiUnknownError clientmsg) `runReaderT` logger
      handshake (Just clientId) cfg conn logger
        >>= either (throwIO . ErrorCall . show) (const $ pure ())

giveUp :: (Show err) => Connection -> Logger -> Maybe WSSessionId -> String -> err -> IO ()
giveUp conn logger mclientId topic err = do
  closeWsConn conn logger mclientId (Just $ topic <> ": " <> show err)

handshake :: Maybe WSSessionId -> Config -> Connection -> Logger -> IO (Either String WSSessionId)
handshake mOldClientId cfg conn logger = (receiveMessage Nothing conn `runReaderT` logger) >>= \case
  TSGreeting mNewClientId -> handshake' mOldClientId cfg conn logger mNewClientId
  bad -> pure . Left $ case mOldClientId of
           Nothing  -> "handshake: expected TCGreeting, got " <> show bad
           Just cid -> "re-handshake on " <> show cid <> ": expected TCGreeting, got " <> show bad

handshake' :: Maybe WSSessionId -> Config -> Connection -> Logger -> Maybe WSSessionId -> IO (Either String WSSessionId)
handshake' mOldClientId cfg conn logger mNewClientId = case mNewClientId of
  -- the client is reconnected, its WSSessionId is n
  Just n -> do
    if maybe True (== n) mOldClientId
      then do
        cmap <- takeMVar webSocketMVar
        putMVar webSocketMVar $ Map.adjust (_2 .~ mempty) n cmap
        sendMessage Nothing conn (TCRestrictKeys []) `runReaderT` logger
            -- TUNING: only send this if we know something happened since the disconnect.
        pure $ Right n
      else do
        pure . Left $ "re-handshake on " <> show (fromJust mOldClientId) <> ": unexpected session id: " <> show n

  -- the first connection of the client
  Nothing -> do
    clientId <- openWsConn cfg conn
    sendMessage Nothing conn (TCGreeting clientId) `runReaderT` logger
    pure $ Right clientId

pingLoop :: HasCallStack => (forall a. AppM DB a -> IO (Either ApiError a)) -> Connection -> IO ()
pingLoop toIO conn = either (error . ("impossible: " <>) . show) (forkPingThread conn . timespanSecs)
                     =<< toIO (asks . view $ appConfig . cfgWSPingPeriod)

cmdLoopWrapRound :: forall m. (m ~ AppM DB) => (forall a. m a -> IO (Either ApiError a))
        -> Config -> Connection -> Logger -> WSSessionId -> IO ()
cmdLoopWrapRound toIO cfg conn logger clientId = dolift . dostate $ cmdLoopRound conn clientId
  where
    dostate :: m LoopRoundResult -> m ()
    dostate cmd = do
      msess <- Map.lookup clientId <$> liftIO (readMVar webSocketMVar)
      case msess of
        Just ((_, appStateMVar), _) -> do
          put =<< liftIO (takeMVar appStateMVar)
          sessvalid <- verifyAppState
          if sessvalid
            then do
              cmd >>= \case
                LoopRoundOk -> do
                  liftIO . putMVar appStateMVar =<< get
                (LoopRoundRequestHandshake mNewClientId) -> do
                  liftIO $ handshake' (Just clientId) cfg conn logger mNewClientId >>= \case
                    Right _ -> pure ()
                    Left err -> throwIO $ ErrorCall err
            else do
              sendMessage (Just clientId) conn (TCError ApiSessionTimedout)
              liftIO $ deleteWsConn clientId

        Nothing -> do
          sendMessage (Just clientId) conn (TCError ApiSessionInvalid)

    dolift :: m () -> IO ()
    dolift cmd = toIO cmd >>= \case
      Right () -> pure ()
      Left (e :: ApiError) -> toIO (sendMessage (Just clientId) conn (TCError e)) >>= \case
        Right () -> pure ()
        Left (e' :: ApiError) -> throwIO . ErrorCall $ "could not send uncaught exception to client! " <> show (e, e')

data LoopRoundResult = LoopRoundOk | LoopRoundRequestHandshake (Maybe WSSessionId)
  deriving (Eq, Show)

-- | Application engine.  From here on inwards, all actions need to be authorization-checked.
cmdLoopRound :: Connection -> WSSessionId -> (MonadWS m, MonadApp m) => m LoopRoundResult
cmdLoopRound conn clientId = do
  let sendmsg = sendMessage (Just clientId) conn
  msg <- receiveMessage (Just clientId) conn
  appLog LogDebug . ("appState = " <>) . show =<< get

  let success = (const LoopRoundOk <$>)
  case msg of
      TSClearCache -> success $ do
          modifyCache clientId $ const mempty
          sendmsg $ TCRestrictKeys []
      TSMissing keys -> success $ do
          modifyCache clientId (<> Set.fromList keys)
          cache <- mconcat <$> mapM getData keys
          sendmsg $ TCServerCache cache

      TSAddGroup cg           -> success $ sendmsg . TCCreatedGroup . view groupID =<< App.addGroup cg
      TSUpdateGroup gid x     -> success . void $ App.modifyGroup gid x
      TSCreateUser cu         -> success $ sendmsg . TCCreateUserResp =<< tryApp (App.createUser cu)
      TSUpdateUser uid x      -> success $ updateUser uid x
      TSLogin li              -> success $ sendmsg . TCLoginResp =<< tryApp (App.login li)
      TSLogout                -> success $ void App.logout
      TSGetTranslations k     -> success $ sendmsg . TCTranslations =<< App.getTranslations k

      TSAddVDoc cv            -> success $ sendmsg . TCCreatedVDoc . view vdocID =<< App.createVDoc cv
      TSUpdateVDoc vid upd    -> success . void $ App.updateVDoc vid upd
      TSAddDiscussion eid x   -> success . void $ App.addDiscussion eid x
      TSAddStatement sid x    -> success . void $ App.addStatement sid x
      TSUpdateStatement sid x -> success . void $ App.updateStatement sid x
      TSAddEdit eid x         -> success . void $ App.addEdit eid x
      TSAddEditAndMerge eid x -> success . void $ App.addEditAndMerge eid x
      TSUpdateEdit eid x      -> success . void $ App.updateEdit eid x
      TSMergeEdit eid         -> success . void $ App.mergeEditAndRebaseAllSiblings eid
      TSToggleVote (ContribIDEdit eid) x -> success $ do
        rebased <- App.toggleSimpleVoteOnEdit eid x
        when rebased $ sendmsg TCRebase
      TSToggleVote (ContribIDDiscussion _ nid) x
                              -> success . void $ App.toggleSimpleVoteOnDiscussion nid x
      TSDeleteVote eid        -> success . void $ App.deleteSimpleVoteOnEdit eid

      TSGreeting mclientId'   -> pure $ LoopRoundRequestHandshake mclientId'
