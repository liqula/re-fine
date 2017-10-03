{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Server
  ( refineCookieName
  , startBackend
  , runCliAppCommand
  , Backend(..), mkBackend
  , refineApi
  ) where
#include "import_backend.hs"

import           Control.Concurrent
import           Network.HTTP.Media ((//))
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai (Middleware)
import qualified Servant.Cookie.Session as SCS
import           Servant.Server.Internal (responseServantErr)
import           Servant.Utils.StaticFiles (serveDirectory)
import           System.Directory (canonicalizePath, createDirectoryIfMissing)
import           System.FilePath (dropFileName)

import Refine.Backend.App as App
import Refine.Backend.App.MigrateDB (migrateDB)
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Types
import Refine.Common.Rest
import Refine.Common.Types


-- * Constants

refineCookieName :: SBS
refineCookieName = "refine"


-- * Initialization

createDataDirectories :: Config -> IO ()
createDataDirectories cfg = do
  case cfg ^. cfgDBKind of
    DBOnDisk path -> do
      cpath <- canonicalizePath path
      createDirectoryIfMissing True (dropFileName cpath)
    DBInMemory -> pure ()


-- * backend creation

data Backend db = Backend
  { backendServer          :: Application
                              -- ^ the http server (static content, web sockets, rest api, client config).
  , backendRunApp          :: AppM db :~> ExceptT ApiError IO
                              -- ^ the application engine.
  , backendLogChan         :: LogChan
  }

refineApi :: (Database db) => ServerT RefineAPI (AppM db)
refineApi =
       App.getVDoc
  :<|> App.createVDoc
  :<|> App.updateVDoc
  :<|> App.addEdit
  :<|> App.getEdit
  :<|> App.mergeEditAndRebaseAllSiblings
  :<|> App.updateEdit
  :<|> App.addDiscussion
  :<|> App.getDiscussion
  :<|> App.addStatement
  :<|> App.updateStatement
  :<|> App.createUser
  :<|> App.getUser
  :<|> App.login
  :<|> App.logout
  :<|> App.getTranslations
  :<|> App.addGroup
  :<|> App.getGroup
  :<|> App.modifyGroup
  :<|> App.getGroups
  :<|> App.changeSubGroup
  :<|> App.changeRole
  :<|> ((void .) . App.toggleSimpleVoteOnEdit)
  :<|> App.deleteSimpleVoteOnEdit
  :<|> App.getSimpleVotesOnEdit


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
  (backend, destroydb) <- mkBackend cfg
  destroylogger <- attachLogger cfg backend
  Warp.runSettings (warpSettings cfg) (backendServer backend)
    `finally` (destroydb >> destroylogger)

runCliAppCommand :: Config -> AppM DB a -> IO ()
runCliAppCommand cfg cmd = do
  (backend, destroydb) <- mkBackend cfg
  destroylogger <- attachLogger cfg backend
  (void $ (natThrowError . backendRunApp backend) $$ cmd)
    `finally` (destroydb >> destroylogger)

-- | Call 'mkLogChan' and plug the log channel from 'Backend' into it.  Return destructor.
attachLogger :: Config -> Backend DB -> IO (IO ())
attachLogger cfg (Backend _ _ logsource) = do
  (logsink, free) <- mkLogChan cfg
  tid <- forkIO . forever $ readChan logsource >>= writeChan logsink
  pure $ killThread tid >> free

mkBackend :: Config -> IO (Backend DB, IO ())
mkBackend cfg = do
  createDataDirectories cfg
  logchan <- newChan

  -- create runners
  (dbRunner, dbNat, destroydb) <- createDBNat cfg
  backend <- mkServerApp cfg logchan dbNat dbRunner

  -- setup actions like migration, intial content, ...
  runExceptT (backendRunApp backend $$ unsafeAsGod (migrateDB cfg))
    >>= either (error . show) (const $ pure ())

  pure (backend, destroydb)

-- | NOTE: Static content delivery is not protected by "Servant.Cookie.Session".  To achive that, we
-- may need to refactor, e.g. by using extra arguments in the end point types.  Iff we only serve
-- the application code as static content, and not content that is subject to authorization, we're
-- fine as it is.
mkServerApp :: Config -> LogChan -> MkDBNat DB -> DBRunner -> IO (Backend DB)
mkServerApp cfg logchan dbNat dbRunner = do
  let cookie = SCS.def { SCS.setCookieName = refineCookieName, SCS.setCookiePath = Just "/" }

      appNT :: AppM DB :~> ExceptT ApiError IO
      appNT = runApp dbNat
                   dbRunner
                   logchan
                   cfg

      renderErrorNT :: ExceptT ApiError IO :~> ExceptT ServantErr IO
      renderErrorNT = NT $ \(ExceptT m) -> ExceptT $ m >>= \case
        (Right v) -> pure $ Right v
        (Left e)  -> pure $ appServantErr e

  srvApp :: Application
      <- if cfg ^. cfgHaveRestApi

           -- with rest api, cookies, everything.
           then fst <$> SCS.serveAction
              (Proxy :: Proxy (RefineAPI :<|> ClientConfigAPI))
              (Proxy :: Proxy AppState)
              cookie
              (Nat liftIO)
              (cnToSn $ renderErrorNT . appNT)
              (refineApi :<|> clientConfigApi (cfg ^. cfgClient))
              (Just (serve (Proxy :: Proxy Raw) (maybeServeDirectory (cfg ^. cfgFileServeRoot))))

           -- just anonymous client code and client config delivery.
           else pure $ serve
              (Proxy :: Proxy (ClientConfigAPI :<|> Raw))
              (clientConfigApi (cfg ^. cfgClient) :<|> maybeServeDirectory (cfg ^. cfgFileServeRoot))

  () <- resetWebSocketMVar

  let addWS :: Middleware
      addWS = startWebSocketServer cfg logchan appMToIO

      appMToIO :: AppM DB a -> IO (Either ApiError a)
      appMToIO m = runExceptT (appNT $$ m)

  pure $ Backend (addWS srvApp) appNT logchan


maybeServeDirectory :: Maybe FilePath -> Server Raw
maybeServeDirectory = maybe (\_ respond -> respond $ responseServantErr err404) serveDirectory

-- | Convert 'ApiError' (internal to backend) to 'ServantError' (passed over HTTP).  Conversion from
-- 'AppError' to 'ApiError' and logging is left to 'tryApp', 'toApiError'.  This function just
-- handles the HTTP-specific stuff like response code and body encoding.
appServantErr :: ApiError -> (Monad m, MonadError ServantErr m) => m a
appServantErr err = throwError $ (appServantErr' err) { errBody = encode err }

appServantErr' :: ApiError -> ServantErr
appServantErr' = \case
  ApiUnknownError          -> err500
  ApiVDocVersionError      -> err409
  ApiDBError dbe           -> dbServantErr dbe
  ApiUserNotFound _        -> err404
  ApiUserNotLoggedIn       -> err403
  ApiUserCreationError uce -> userCreationError uce
  ApiCsrfError _           -> err403
  ApiSessionInvalid        -> err403
  ApiSanityCheckError _    -> err409
  ApiL10ParseErrors _      -> err500
  ApiUnauthorized _        -> err403
  ApiMergeError{}          -> err500
  ApiRebaseError{}         -> err500
  ApiSmtpError{}           -> err500
  ApiTimeoutError{}        -> err500

dbServantErr :: ApiErrorDB -> ServantErr
dbServantErr = \case
  ApiDBUnknownError       _ -> err500
  ApiDBNotFound           _ -> err404
  ApiDBNotUnique          _ -> err409
  ApiDBException          _ -> err500
  ApiDBUserNotLoggedIn      -> err403
  ApiDBMigrationParseErrors -> err500
  ApiDBUnsafeMigration      -> err500

userCreationError :: ApiErrorCreateUser -> ServantErr
userCreationError = \case
  ApiErrorInvalidPassword              -> err400
  ApiErrorUsernameAlreadyTaken         -> err409
  ApiErrorEmailAlreadyTaken            -> err409
  ApiErrorUsernameAndEmailAlreadyTaken -> err409


-- * Instances for Servant.Cookie.Session

instance SCS.MonadRandom (AppM db) where
  getRandomBytes = liftIO . SCS.getRandomBytes

instance SCS.ThrowError500 AppError where
  error500 = prism' toAppError fromAppError
    where
      toAppError :: String -> AppError
      toAppError = AppCsrfError . cs

      fromAppError :: AppError -> Maybe String
      fromAppError (AppCsrfError e) = Just $ cs e
      fromAppError _                = Nothing

instance SCS.HasSessionCsrfToken AppState where
  sessionCsrfToken = appCsrfToken . csrfTokenIso
    where
      fromSCS = CsrfToken . cs . SCS.fromCsrfToken
      toSCS   = SCS.CsrfToken . cs . _csrfToken
      csrfTokenIso = iso (fmap toSCS) (fmap fromSCS)

instance SCS.GetCsrfSecret AppContext where
  csrfSecret = appConfig . cfgCsrfSecret
             . to (Refine.Backend.Types.CsrfSecret . cs)
             . Refine.Backend.Types.csrfSecret
             . to (Just . SCS.CsrfSecret . cs)
    -- (FUTUREWORK: this looks like Config should not carry around the string, but one of the many
    -- types alled 'CsrfSecret'.  but i'm not going to investigate this now.)

instance SCS.GetSessionToken AppState where
  getSessionToken = appUserState . to (\case
    UserLoggedIn _user session -> Just . SCS.SessionToken $ userSessionText session
    UserLoggedOut              -> Nothing)
