{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | (This module is called "Cache" for historical reasons.  It's really more of a "WebSocket"
-- protocol module, perhaps we should rename it.)
module Refine.Backend.App.Cache
  ( startWebSocketServer
  , resetWebSocketMVar
  ) where
#include "import_backend.hs"

import           Control.Concurrent
import           Network.Wai (Middleware)
import           System.IO.Unsafe
import           Network.WebSockets
import           Network.Wai.Handler.WebSockets
import           Crypto.Random.Entropy

import Refine.Common.Types
import Refine.Common.Rest (ApiError)
import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import {-# SOURCE #-} Refine.Backend.App.Translation as App (getTranslations)
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger


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

openWsConn :: Config -> Connection -> IO WSSessionId
openWsConn cfg conn = do
  appState <- newMVar $ initialAppState cfg
  modifyMVar webSocketMVar $ \cmap -> do
    rnd <- getEntropy 512
    time <- getCurrentTimestamp
    let i = WSSessionId $ cs (rnd :: SBS) <> cs (show time)
        cmap' = Map.insert i ((conn, appState), mempty) cmap
    pure (cmap', i)

closeWsConn :: WSSessionId -> IO ()
closeWsConn n = do
  modifyMVar webSocketMVar $ \cmap ->do
    pure (Map.delete n cmap, ())


class MonadWS m where
  sendMessage :: Connection -> ToClient -> m ()
  receiveMessage :: Connection -> m ToServer
  modifyCache :: WSSessionId -> (Set CacheKey -> Set CacheKey) -> m ()

instance MonadIO m => MonadWS m where
  sendMessage conn msg = liftIO $ sendTextData conn (cs $ encode msg :: ST)
  receiveMessage conn = liftIO $ receiveData conn <&> fromMaybe (error "websockets json decoding error") . decode
  modifyCache sid f = liftIO . modifyMVar webSocketMVar $ \m -> pure (Map.adjust (second f) sid m, ())


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
        appLog LogDebug $ "invalidate cache items " <> show (Set.toList keys)
        forM_ cls $ \(n, conn, d) -> do
            liftIO . sendMessage conn . TCInvalidateKeys $ Set.toList d
            appLog LogDebug $ "invalidate cache of client #" <> show n <> ": " <> show (Set.toList d)
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
      clientId <- handshake cfg conn (mkLogger cfg)
      forever (cmdLoopStepFrame toIO clientId (cmdLoopStep conn clientId)
                `catch` handleAllErrors cfg conn (mkLogger cfg) clientId)
        `catch` giveUp conn (mkLogger cfg) clientId  -- FUTUREWORK: handle exceptions during inital handshake?

handleAllErrors :: Config -> Connection -> Logger -> WSSessionId -> SomeException -> IO ()
handleAllErrors cfg conn (Logger logger) clientId e = do
  logger LogError $ show e
  sendMessage conn TCReset
  clientId' <- handshake cfg conn (Logger logger)
  assert (clientId' == clientId) $ pure ()

giveUp :: Connection -> Logger -> WSSessionId -> SomeException -> IO ()
giveUp conn (Logger logger) clientId e = do
  logger LogError . show . WSErrorResetFailed $ show e
  closeWsConn clientId
  sendClose conn ("Cache.hs: could not reset connection, giving up." :: ST)
    -- (FUTUREWORK: we should flush any remaining incoming messages here; see 'sendClose'.)


data WSError
  = WSErrorUnexpectedPacket ToServer
  | WSErrorCommandFailed ApiError
  | WSErrorUnknownError String
  | WSErrorResetFailed String
  deriving (Eq, Show)

wserror :: Monad m => WSError -> m a
wserror = error . show


handshake :: Config -> Connection -> Logger -> IO WSSessionId
handshake cfg conn (Logger logger) = receiveMessage conn >>= \case
  -- the client is reconnected, its WSSessionId is n
  TSGreeting (Just n) -> do
    cmap <- takeMVar webSocketMVar
    putMVar webSocketMVar $ Map.adjust (_2 .~ mempty) n cmap
    sendMessage conn $ TCRestrictKeys []  -- TUNING: only send this if we know something happened since the disconnect.
    logger LogDebug $ "websocket client #" <> show n <> " is reconnected"
    pure n

  -- the first connection of the client
  TSGreeting Nothing -> do
    n <- openWsConn cfg conn
    sendMessage conn $ TCGreeting n
    logger LogDebug $ "new websocket client #" <> show n
    pure n

  bad -> wserror $ WSErrorUnexpectedPacket bad

pingLoop :: HasCallStack => (forall a. AppM DB a -> IO (Either ApiError a)) -> Connection -> IO ()
pingLoop toIO conn = either (error . show) (forkPingThread conn . timespanSecs)
                     =<< toIO (asks . view $ appConfig . cfgWSPingPeriod)

cmdLoopStepFrame :: forall m. (m ~ AppM DB ()) => (m -> IO (Either ApiError ())) -> WSSessionId -> m -> IO ()
cmdLoopStepFrame toIO clientId = dolift . dostate
  where
    dostate :: m -> m
    dostate cmd = do
      Just ((_, appStateMVar), _) <- Map.lookup clientId <$> liftIO (readMVar webSocketMVar)
      put =<< liftIO (takeMVar appStateMVar)
      cmd
      liftIO . putMVar appStateMVar =<< get

    dolift :: m -> IO ()
    dolift cmd = toIO cmd >>= \case
      Left e -> wserror $ WSErrorCommandFailed e
      Right () -> pure ()


-- | Application engine.  From here on inwards, all actions need to be authorization-checked.
cmdLoopStep :: Connection -> WSSessionId -> (MonadWS m, MonadApp m) => m ()
cmdLoopStep conn clientId = do
  msg <- receiveMessage conn
  appLog LogDebug $ "request from client #" <> show clientId <> ", " <> show msg
  appLog LogDebug . ("appState = " <>) . show =<< get

  case msg of
      TSClearCache -> do
          modifyCache clientId $ const mempty
          sendMessage conn $ TCRestrictKeys []
      TSMissing keys -> do
          modifyCache clientId (<> Set.fromList keys)
          cache <- mconcat <$> mapM getData keys
          sendMessage conn $ TCServerCache cache

      TSAddGroup cg           -> sendMessage conn . TCCreatedGroup . view groupID =<< App.addGroup cg
      TSUpdateGroup gid x     -> void $ App.modifyGroup gid x
      TSCreateUser cu         -> sendMessage conn . TCCreateUserResp =<< tryApp (App.createUser cu)
      TSUpdateUser uid x      -> updateUser uid x
      TSLogin li              -> sendMessage conn . TCLoginResp =<< tryApp (App.login li)
      TSLogout                -> void App.logout
      TSGetTranslations k     -> sendMessage conn . TCTranslations =<< App.getTranslations k

      TSAddVDoc cv            -> sendMessage conn . TCCreatedVDoc . view vdocID =<< App.createVDoc cv
      TSUpdateVDoc vid upd    -> void $ App.updateVDoc vid upd
      TSAddDiscussion eid x   -> void $ App.addDiscussion eid x
      TSAddStatement sid x    -> void $ App.addStatement sid x
      TSUpdateStatement sid x -> void $ App.updateStatement sid x
      TSAddEdit eid x         -> void $ App.addEdit eid x
      TSAddEditAndMerge eid x -> void $ App.addEditAndMerge eid x
      TSUpdateEdit eid x      -> void $ App.updateEdit eid x
      TSMergeEdit eid         -> void $ App.mergeEditAndRebaseAllSiblings eid
      TSToggleVote (ContribIDEdit eid) x -> do
        rebased <- App.toggleSimpleVoteOnEdit eid x
        when rebased $ sendMessage conn TCRebase
      TSToggleVote (ContribIDDiscussion _ nid) x
                              -> void $ App.toggleSimpleVoteOnDiscussion nid x
      TSDeleteVote eid        -> void $ App.deleteSimpleVoteOnEdit eid

      bad@(TSGreeting _)      -> wserror $ WSErrorUnexpectedPacket bad
