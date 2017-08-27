{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Server
  ( refineCookieName
  , startBackend
  , runCliAppCommand
  , Backend(..), mkProdBackend
  , refineApi
  ) where
#include "import_backend.hs"

import           Control.Concurrent.MVar
import           Network.HTTP.Media ((//))
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai (Middleware)
import qualified Network.Wai.Session as SCS
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
  , backendRunApp          :: AppM db :~> ExceptT ApiError IO
  , backendSessionStore    :: SCS.SessionStore IO () (AppState, MVar ())
  }

refineApi :: (Database db) => ServerT RefineAPI (AppM db)
refineApi =
       App.getVDoc
  :<|> App.createVDoc
  :<|> App.updateVDoc
  :<|> App.addEdit
  :<|> App.getEdit
  :<|> App.mergeEdit
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

type ClientConfigAPI = "cfg.js" :> Get '[JSViaRest, JSON] ClientCfg

-- | Serve 'ClientCfg' as a js file for import in index.html.
clientConfigApi :: (Database db) => ServerT ClientConfigAPI (AppM db)
clientConfigApi = asks . view $ appConfig . cfgClient


startBackend :: Config -> IO ()
startBackend cfg = do
  (backend, _destroy) <- mkProdBackend cfg
  Warp.runSettings (warpSettings cfg) $ backendServer backend

runCliAppCommand :: Config -> AppM DB a -> IO ()
runCliAppCommand cfg cmd = do
  (backend, _destroy) <- mkProdBackend cfg
  void $ (natThrowError . backendRunApp backend) $$ cmd

mkProdBackend :: Config -> IO (Backend DB, IO ())
mkProdBackend cfg = mkBackend cfg $ do
  () <- migrateDB cfg
  gs <- App.getGroups
  when (null gs) $ do
    void . addGroup $ CreateGroup "default" "default group" [] [] mempty
                                                                   -- FIXME: don't do that any more!
                                                                   -- we have `--init` and
                                                                   -- `initializeDB` now!
  pure ()

mkBackend :: Config -> AppM DB a -> IO (Backend DB, IO ())
mkBackend cfg initially = do
  createDataDirectories cfg

  -- create runners
  (dbRunner, dbNat, destroy) <- createDBNat cfg
  backend <- mkServerApp cfg dbNat dbRunner

  -- setup actions like migration, intial content, ...
  result <- runExceptT (backendRunApp backend $$ unsafeAsGod initially)
  either (error . show) (const $ pure ()) result

  pure (backend, destroy)

mkServerApp :: Config -> MkDBNat DB -> DBRunner -> IO (Backend DB)
mkServerApp cfg dbNat dbRunner = do
  let cookie = SCS.def { SCS.setCookieName = refineCookieName, SCS.setCookiePath = Just "/" }
      logger = defaultLogger cfg

      appNT :: AppM DB :~> ExceptT ApiError IO
      appNT = runApp dbNat
                   dbRunner
                   logger
                   cfg

      renderErrorNT :: ExceptT ApiError IO :~> ExceptT ServantErr IO
      renderErrorNT = NT $ \(ExceptT m) -> ExceptT $ m >>= \case
        (Right v) -> pure $ Right v
        (Left e)  -> pure $ appServantErr e

  -- FIXME: Static content delivery is not protected by "Servant.Cookie.Session" To achive that, we
  -- may need to refactor, e.g. by using extra arguments in the end point types.
  (srvApp :: Application, sessionStore :: SCS.SessionStore IO () (SCS.Lockable AppState))
      <- SCS.serveAction
              (Proxy :: Proxy (RefineAPI :<|> ClientConfigAPI))
              (Proxy :: Proxy AppState)
              cookie
              (Nat liftIO)
              (cnToSn $ renderErrorNT . appNT)
              (refineApi :<|> clientConfigApi)
              (Just (serve (Proxy :: Proxy Raw) (maybeServeDirectory (cfg ^. cfgFileServeRoot))))

  () <- resetWebSocketMVar

  let addWS :: Middleware
      addWS = startWebSocketServer cfg appMToIO

      appMToIO :: AppM DB a -> IO (Either ApiError a)
      appMToIO m = runExceptT (appNT $$ m)

  pure $ Backend (addWS srvApp) appNT sessionStore


maybeServeDirectory :: Maybe FilePath -> Server Raw
maybeServeDirectory = maybe (\_ respond -> respond $ responseServantErr err404) serveDirectory

-- | Convert 'ApiError' (internal to backend) to 'ServantError' (passed over HTTP).  Conversion from
-- 'AppError' to 'ApiError' and logging is left to 'tryApp', 'toApiError'.  This function just
-- handles the HTTP-specific stuff like response code and body encoding.
appServantErr :: ApiError -> (Monad m, MonadError ServantErr m) => m a
appServantErr err = throwError $ (appServantErr' err) { errBody = encode err }

appServantErr' :: ApiError -> ServantErr
appServantErr' = \case
  ApiUnknownError _        -> err500
  ApiVDocVersionError      -> err409
  ApiDBError dbe           -> dbServantErr dbe
  ApiUserNotFound _        -> err404
  ApiUserNotLoggedIn       -> err403
  ApiUserCreationError uce -> userCreationError uce
  ApiCsrfError _           -> err403
  ApiSessionError          -> err403
  ApiSanityCheckError _    -> err409
  ApiUserHandleError _     -> err500
  ApiL10ParseErrors _      -> err500
  ApiUnauthorized _        -> err403
  ApiMergeError{}          -> err500
  ApiRebaseError{}         -> err500
  ApiSmtpError{}           -> err500

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
