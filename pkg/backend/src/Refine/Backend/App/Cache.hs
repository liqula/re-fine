{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | FIXME: This module is called "Cache" for historical reasons.  It's really more of a "WebSocket"
-- protocol module, perhaps we should rename it.
module Refine.Backend.App.Cache
  ( startWebSocketServer
  , resetWebSocketMVar
  -- (also exports @instance Database db => MonadCache (AppM db)@)

    -- * exported for testing only
  , mkWSSessionId
  ) where
#include "import_backend.hs"

import           Control.Exception
import           Control.Concurrent
import qualified "cryptonite" Crypto.Hash as CRT
import qualified Crypto.Random.Entropy as CRT
import           Network.Wai.Handler.WebSockets
import           Network.Wai (Middleware)
import           Network.WebSockets
import           Network.WebSockets.Connection
import           System.IO.Unsafe
import           System.Timeout (timeout)

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


-- * types

type WebSocketMVar = MVar (Map WSSessionId WebSocketSession)
type WebSocketSession = ((Connection, MVar AppState), Set CacheKey)

newtype WSError = WSErrorExpectedTSGreeting ToServer
  deriving (Eq, Show)


-- * session store

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


-- * monad

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

logWSMessage :: (Monad m, MonadLog m, MonadWS m, Show a) => Maybe WSSessionId -> a -> m ()
logWSMessage mclientId msg = appLog LogDebug $
  cs ("[" <> maybe "no session" (ST.take 10 . unWSSessionId) mclientId <> "] ") <>
  show msg


-- * server

startWebSocketServer :: Config -> (forall a. AppM DB a -> IO (Either ApiError a)) -> Middleware
startWebSocketServer cfg toIO = websocketsOr options server
  where
    options :: ConnectionOptions
    options = defaultConnectionOptions

    logger :: Logger
    logger = mkLogger cfg

    server :: ServerApp
    server pendingconnection = handle (handleHandshakeException logger) $ do
      conn <- acceptRequest pendingconnection
      (do
        pingLoop toIO conn
        eClientId <- handshake cfg conn logger
        case eClientId of
          Right clientId ->
            forever (stepWrapper toIO conn clientId (step conn clientId)
                      >>= \case Left err -> handleUncaughtApiError conn logger err
                                Right () -> pure ())
          Left err -> handleHandshakeError conn logger err)
        `catch` handleConnectionException conn logger
        `catch` handleAnyException conn logger


-- | thrown by 'acceptRequest'.
handleHandshakeException :: Logger -> HandshakeException -> IO ()
handleHandshakeException logger err = do
  unLogger logger LogInfo $ show err

handleUncaughtApiError :: Connection -> Logger -> ApiError -> IO ()
handleUncaughtApiError conn logger err = do
  sendMessage Nothing conn (TCError err) `runReaderT` logger

-- | thrown by 'handshake' below.
handleHandshakeError :: Connection -> Logger -> WSError -> IO ()
handleHandshakeError conn logger = giveup conn logger Nothing

-- | FUTUREWORK: sometimes, there is a clientId here, but it's a little awkward to get access to it.
handleConnectionException :: Connection -> Logger -> ConnectionException -> IO ()
handleConnectionException conn logger = giveup conn logger Nothing

handleAnyException :: Connection -> Logger -> SomeException -> IO ()
handleAnyException conn logger = giveup conn logger Nothing

-- | Given an error, log it and close the connection.  (Otherwise the client may keep waiting for us
-- to say something.)  Note that we do not gc sessions here.  The client can still open a new
-- connection and connect with the old clientId.
giveup :: (Show err) => Connection -> Logger -> Maybe WSSessionId -> err -> IO ()
giveup conn logger mClientId err = do
  unLogger logger LogInfo $ "giveup: " <> show (mClientId, err)
  void . forkIO . dropExceptions $ do
    -- code 1011: "unexpected condition" http://tools.ietf.org/html/rfc6455#section-7.4
    sendCloseCode @ST conn 1011 "bye"
    void . timeout (timespanUs $ TimespanSecs 45) $ do
      forever receiveDataMessage conn

dropExceptions :: IO () -> IO ()
dropExceptions = handle (\(SomeException _) -> pure ())


-- * handshake

handshake :: Config -> Connection -> Logger -> IO (Either WSError WSSessionId)
handshake cfg conn logger = (receiveMessage Nothing conn `runReaderT` logger) >>= \case
  -- the client is reconnected, its WSSessionId is n
  TSGreeting (Just clientId) -> do
    cmap <- takeMVar webSocketMVar
    putMVar webSocketMVar $ Map.adjust (_2 .~ mempty) clientId cmap
    sendMessage Nothing conn (TCRestrictKeys []) `runReaderT` logger
        -- TUNING: only send this if we know something happened since the disconnect.
    pure $ Right clientId

  -- the first connection of the client
  TSGreeting Nothing -> do
    clientId <- openWsConn cfg conn
    sendMessage Nothing conn (TCGreeting clientId) `runReaderT` logger
    pure $ Right clientId

  bad -> pure . Left . WSErrorExpectedTSGreeting $ bad

pingLoop :: HasCallStack => (forall a. AppM DB a -> IO (Either ApiError a)) -> Connection -> IO ()
pingLoop toIO conn = either (error . show) (forkPingThread conn . timespanSecs)
                     =<< toIO (asks . view $ appConfig . cfgWSPingPeriod)


-- * MonadApp

stepWrapper :: forall m. (m ~ AppM DB ())
                 => (m -> IO (Either ApiError ())) -> Connection -> WSSessionId
                 -> m -> IO (Either ApiError ())
stepWrapper toIO conn clientId = toIO . dostate
  where
    dostate :: m -> m
    dostate cmd = do
      Just ((_, appStateMVar), _) <- Map.lookup clientId <$> liftIO (readMVar webSocketMVar)
      put =<< liftIO (takeMVar appStateMVar)
      sessvalid <- verifyAppState
      unless sessvalid $ sendMessage (Just clientId) conn TCLogout  -- TODO: or TCError, rather?
      cmd
      liftIO . putMVar appStateMVar =<< get


-- | Application engine.  From here on inwards, all actions need to be authorization-checked.
step :: Connection -> WSSessionId -> (MonadWS m, MonadApp m) => m ()
step conn clientId = do
  let sendmsg = sendMessage (Just clientId) conn
  msg <- receiveMessage (Just clientId) conn
  appLog LogDebug . ("appState = " <>) . show =<< get

  case msg of
      TSClearCache -> do
          modifyCache clientId $ const mempty
          sendmsg $ TCRestrictKeys []
      TSMissing keys -> do
          modifyCache clientId (<> Set.fromList keys)
          cache <- mconcat <$> mapM getData keys
          sendmsg $ TCServerCache cache

      TSAddGroup cg           -> sendmsg . TCCreatedGroup . view groupID =<< App.addGroup cg
      TSUpdateGroup gid x     -> void $ App.modifyGroup gid x
      TSCreateUser cu         -> sendmsg . TCCreateUserResp =<< tryApp (App.createUser cu)
      TSUpdateUser uid x      -> updateUser uid x
      TSLogin li              -> sendmsg . TCLoginResp =<< tryApp (App.login li)
      TSLogout                -> void App.logout
      TSGetTranslations k     -> sendmsg . TCTranslations =<< App.getTranslations k

      TSAddVDoc cv            -> sendmsg . TCCreatedVDoc . view vdocID =<< App.createVDoc cv
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
        when rebased $ sendmsg TCRebase
      TSToggleVote (ContribIDDiscussion _ nid) x
                              -> void $ App.toggleSimpleVoteOnDiscussion nid x
      TSDeleteVote eid        -> void $ App.deleteSimpleVoteOnEdit eid

      bad@(TSGreeting _) -> throwError . AppUnknownError $ "unexpected package: " <> cs (show bad)
        -- FUTUREWORK: this should be a WSError, but we can't easily throw this here.  could we have
        -- a constraint set here with two exception types, distinguishable by their error type?
        -- then we could throw 'WSError' here and extend that with a second error constructor.


-- * cache

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
