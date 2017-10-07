{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Server
  ( startBackend
  , runCliAppCommand
  , mkBackend

  , Backend(..)
  , backendServer
  , backendRunApp
  , backendLogChan
  , backendWSSessionMap
  , backendCacheInvalidateQ
  , backendCacheInvalidateS
  , backendSessionGCS

  , wssAppState
  , wssToClientQ
  , wssLastSeen

  , clientChanSink
  ) where
#include "import_backend.hs"

import Network.HTTP.Media ((//))
import Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware)

import Refine.Backend.App as App
import Refine.Backend.App.MigrateDB (migrateDB)
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Server.Types
import Refine.Backend.Server.WebSocket
import Refine.Common.Types


-- * services

-- | Create 'Sqlite3OnDisk' directory.  (If not applicable, do nothing.)
createDataDirectories :: Config -> IO ()
createDataDirectories cfg = do
  case cfg ^. cfgDBKind of
    Sqlite3OnDisk path -> do
      cpath <- canonicalizePath path
      createDirectoryIfMissing True (dropFileName cpath)
    _ -> pure ()


data JSViaRest

instance Accept JSViaRest where
    contentType Proxy = "application" // "javascript"

instance MimeRender JSViaRest ClientCfg where
  mimeRender Proxy = ("window.client_cfg = " <>) . (<> ";") . encode

type ClientConfigAPI = "cfg.js" :> Get '[JSViaRest, JSON] (CacheBust ClientCfg)

-- | Serve 'ClientCfg' as a js file for import in index.html.
clientConfigApi :: Applicative m => ClientCfg -> ServerT ClientConfigAPI m
clientConfigApi = pure . cacheBust

type CacheBust = Headers '[Header "Cache-Control" ST, Header "Expires" ST]

cacheBust :: a -> CacheBust a
cacheBust = addHeader "no-cache, no-store, must-revalidate" . addHeader "0"


startBackend :: Config -> IO ()
startBackend cfg = do
  (backend, destroydb) <- mkBackend False cfg
  asyncLogger <- attachLogger cfg (backend ^. backendLogChan)
  Warp.runSettings (warpSettings cfg) (backend ^?! backendServer . _Right)
    `finally` (destroydb >> cancel asyncLogger)

-- | run App action via command line, e.g. for schaffolding.  any 'ToClient' messages sent by the
-- action will be logged ('LogInfo') and dropped.
runCliAppCommand :: Config -> AppM DB a -> IO ()
runCliAppCommand cfg cmd = do
  (backend, destroyDb)            <- mkBackend False cfg  -- FIXME: this shouldn't start any network
                                                          -- services!  Backend needs to be more
                                                          -- configurable, appearently.
  asyncLogger                     <- attachLogger cfg (backend ^. backendLogChan)
  (clientChan, destroyClientChan) <- clientChanSink (backend ^. backendLogChan)
  void (runApp' (backend ^. backendRunApp) clientChan cmd)
    `finally` (destroyDb >> destroyClientChan >> cancel asyncLogger)

-- | Call 'mkLogChan' and plug the log channel from 'Backend' into it.  Return destructor.
attachLogger :: Config -> LogChan -> IO (Async ())
attachLogger cfg logsource = do
  (logsink, free) <- mkLogChan cfg
  asyncChanPipe' logsource (atomically . writeTChan logsink) free

-- | FUTUREWORK: this can probably be done more nicely using the @with*@ pattern.  and it should
-- move to some util module together with 'asyncChanPipe'.
clientChanSink :: LogChan -> IO (TChan ToClient, IO ())
clientChanSink sink = do
  source <- newTChanIO
  as <- asyncChanPipe source (atomically . writeTChan sink . (LogInfo,) . show)
  pure (source, cancel as)


mkBackend :: Bool -> Config -> IO (Backend DB, IO ())
mkBackend plainWebSocks cfg = do
  createDataDirectories cfg
  logchan <- newTChanIO
  cachechan <- newTChanIO

  -- create runners
  (dbRunner, dbNat, destroydb) <- createDBNat cfg
  backend :: Backend DB <- mkServerApp plainWebSocks cfg logchan cachechan dbNat dbRunner

  -- setup actions like migration, intial content, ...
  ([], Right ()) <- runAppTC (backend ^. backendRunApp) (unsafeAsGod (migrateDB cfg))

  let free = do
        destroydb
        cancel `mapM_` [ backend ^. backendCacheInvalidateS
                       , backend ^. backendSessionGCS
                       ]
        either cancel (const $ pure ()) $ backend ^. backendServer
  pure (backend, free)


-- | NOTE: Static content delivery is not protected by "Servant.Cookie.Session".  To achive that, we
-- may need to refactor, e.g. by using extra arguments in the end point types.  Iff we only serve
-- the application code as static content, and not content that is subject to authorization, we're
-- fine as it is.
mkServerApp :: Bool -> Config -> LogChan -> TChan (Set CacheKey) -> MkDBNat DB -> DBRunner -> IO (Backend DB)
mkServerApp plainWebSocks cfg logchan cachechan dbNat dbRunner = do
  wssessionmap <- newMVar mempty

  let rapp :: RunApp DB
      rapp = RunApp cfg dbNat dbRunner logchan cachechan

  ciserv <- websocketCacheInvalidationService wssessionmap cachechan
  gcserv <- websocketGCService cfg wssessionmap

  let srvApp :: Application
      srvApp = serve
              (Proxy :: Proxy (ClientConfigAPI :<|> Raw))
              (clientConfigApi (cfg ^. cfgClient) :<|> serveDirectoryWebApp (cfg ^. cfgFileServeRoot))

      addWS :: Middleware
      addWS = websocketMiddleware cfg logchan rapp wssessionmap

  srv :: Either (Async ()) Application
    <- if plainWebSocks
         then Left <$> websocketServer cfg logchan rapp wssessionmap
         else pure . Right $ addWS srvApp

  pure $ Backend srv rapp logchan wssessionmap cachechan ciserv gcserv
