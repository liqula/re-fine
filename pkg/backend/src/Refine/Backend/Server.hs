{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Server
  ( refineCookieName
  , startBackend
  , Backend(..), mkProdBackend, mkDevModeBackend
  , refineApi
  ) where

import           Control.Category
import           Control.Lens ((^.), iso, prism', to)
import           Control.Monad.Except
import qualified Control.Natural as CN
import           Control.Natural (($$))
import           Data.Aeson (encode)
import           Data.String.Conversions (SBS, cs)
import           Debug.Trace (traceShow)  -- (please keep this until we have better logging)
import           Network.Wai.Handler.Warp as Warp
import           Prelude hiding ((.), id)
import           Servant
import qualified Servant.Cookie.Session as SCS
import           Servant.Cookie.Session (serveAction)
import           Servant.Server.Internal (responseServantErr)
import           Servant.Utils.StaticFiles (serveDirectory)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (dropFileName)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database (Database, DB, DBError(..), createDBRunner)
import Refine.Backend.DocRepo (DocRepoError(..), createRunRepo)
import Refine.Backend.DevMode (mockLogin)
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Types
import Refine.Backend.User.Core (CreateUserError(..))
import Refine.Backend.User  hiding (migrateDB)
import Refine.Backend.User.Class (UserHandle) -- FIXME: Reexport
import Refine.Common.Rest
import Refine.Prelude (leftToError)


-- * Constants

refineCookieName :: SBS
refineCookieName = "refine"

-- * Initialization

createDataDirectories :: Config -> IO ()
createDataDirectories cfg = do
  createDirectoryIfMissing True (cfg ^. cfgReposRoot)
  case cfg ^. cfgDBKind of
    DBInMemory    -> pure ()
    DBOnDisk path -> createDirectoryIfMissing True (dropFileName path)


-- * backend creation

data Backend db uh = Backend
  { backendServer :: Application
  , backendMonad  :: AppM db uh CN.:~> ExceptT AppError IO -- TODO: Rename backendRunApp
  }

refineApi :: (Monad db, Database db, Monad uh, UserHandle uh)
          => ServerT RefineAPI (AppM db uh)
refineApi =
       Refine.Backend.App.listVDocs
  :<|> Refine.Backend.App.getCompositeVDoc
  :<|> Refine.Backend.App.createVDocGetComposite
  :<|> Refine.Backend.App.addEdit
  :<|> Refine.Backend.App.addNote
  :<|> Refine.Backend.App.addQuestion
  :<|> Refine.Backend.App.addAnswer
  :<|> Refine.Backend.App.addDiscussion
  :<|> Refine.Backend.App.addStatement
  :<|> Refine.Backend.App.createUser
  :<|> Refine.Backend.App.changeAccess
  :<|> Refine.Backend.App.login
  :<|> Refine.Backend.App.logout

startBackend :: Config -> IO ()
startBackend cfg =
  if cfg ^. cfgDevMode
    then do backend <- mkDevModeBackend cfg mockLogin
            Warp.runSettings (warpSettings cfg) $ backendServer backend
    else do backend <- mkProdBackend cfg
            Warp.runSettings (warpSettings cfg) $ backendServer backend

mkProdBackend :: Config -> IO (Backend DB UH)
mkProdBackend cfg = do
  createDataDirectories cfg

  (runDb, userHandler) <- createDBRunner cfg
  runDocRepo <- createRunRepo cfg
  backend    <- mkServerApp cfg runDb runDocRepo (runUH userHandler)

  when (cfg ^. cfgShouldMigrate) $ do
    void $ (natThrowError . (backendMonad backend)) $$ do
      migrateDB

  pure backend

mkDevModeBackend :: (UserHandleM uh) => Config -> RunUH uh -> IO (Backend DB uh)
mkDevModeBackend cfg runUh = do
  createDataDirectories cfg

  (runDb, _) <- createDBRunner cfg
  runDocRepo <- createRunRepo cfg
  backend    <- mkServerApp cfg runDb runDocRepo runUh

  when (cfg ^. cfgShouldMigrate) $ do
    void $ (natThrowError . (backendMonad backend)) $$ do
      migrateDB'

  pure backend


mkServerApp
    :: (Monad db, Database db, Monad uh, UserHandle uh)
    => Config -> RunDB db -> RunDocRepo -> RunUH uh -> IO (Backend db uh)
mkServerApp cfg runDb runDocRepo runUh = do
  let cookie = SCS.def { SCS.setCookieName = refineCookieName, SCS.setCookiePath = Just "/" }
      logger = Logger $ if cfg ^. cfgShouldLog then putStrLn else const $ pure ()
      app    = runApp runDb
                      runDocRepo
                      runUh
                      logger
                      (cfg ^. cfgCsrfSecret . to CsrfSecret)
                      (cfg ^. cfgSessionLength)
                      (if cfg ^. cfgDevMode then devMode else id)

  -- FIXME: Static content delivery is not protected by "Servant.Cookie.Session" To achive that, we
  -- may need to refactor, e.g. by using extra arguments in the end point types.
  srvApp <- serveAction
              (Proxy :: Proxy RefineAPI)
              (Proxy :: Proxy AppState)
              cookie
              (Nat appIO)
              (toServantError . cnToSn app)
              refineApi
              (Just (Servant.serve (Proxy :: Proxy Raw) (maybeServeDirectory (cfg ^. cfgFileServeRoot))))

  pure $ Backend srvApp app

maybeServeDirectory :: Maybe FilePath -> Server Raw
maybeServeDirectory = maybe (\_ respond -> respond $ responseServantErr err404) serveDirectory

{-# ANN toServantError ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
toServantError :: (Monad m) => ExceptT AppError m :~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> leftToError fromAppError)
  where
    -- FIXME: some (many?) of these shouldn't be err500.
    -- FIXME: implement better logging.
    fromAppError :: AppError -> ServantErr
    fromAppError err = traceShow err $ (appServantErr err) { errBody = encode $ toApiError err }

toApiError :: AppError -> ApiError
toApiError = \case
  AppUnknownError e      -> ApiUnknownError e
  AppVDocError cre       -> ApiVDocError cre
  AppDBError e           -> ApiDBError . cs $ show e
  AppDocRepoError e      -> ApiDocRepoError . cs $ show e
  AppUserNotFound e      -> ApiUserNotFound e
  AppUserNotLoggedIn     -> ApiUserNotLoggedIn
  AppUserCreationError e -> ApiUserCreationError . cs $ show e
  AppCsrfError e         -> ApiCsrfError e
  AppSessionError        -> ApiSessionError
  AppSanityCheckError e  -> ApiSanityCheckError e
  AppUserHandleError e   -> ApiUserHandleError . cs $ show e

-- | Turns AppError to its kind of servant error.
appServantErr :: AppError -> ServantErr
appServantErr = \case
  AppUnknownError _        -> err500
  AppVDocError _           -> err409
  AppDBError dbe           -> dbServantErr dbe
  AppDocRepoError dre      -> docRepoServantErr dre
  AppUserNotFound _        -> err404
  AppUserNotLoggedIn       -> err403
  AppUserCreationError uce -> userCreationError uce
  AppCsrfError _           -> err403
  AppSessionError          -> err403
  AppSanityCheckError _    -> err409
  AppUserHandleError _     -> err500

dbServantErr :: DBError -> ServantErr
dbServantErr = \case
  DBUnknownError    _ -> err500
  DBNotFound        _ -> err404
  DBNotUnique       _ -> err409
  DBException       _ -> err500
  DBUserNotLoggedIn   -> err403

docRepoServantErr :: DocRepoError -> ServantErr
docRepoServantErr = \case
  DocRepoUnknownError _ -> err500
  DocRepoException    _ -> err500

userCreationError :: CreateUserError -> ServantErr
userCreationError = \case
  InvalidPassword              -> err400
  UsernameAlreadyTaken         -> err409
  EmailAlreadyTaken            -> err409
  UsernameAndEmailAlreadyTaken -> err409

-- * Instances for Servant.Cookie.Session

instance SCS.MonadRandom (AppM db uh) where
  getRandomBytes = appIO . SCS.getRandomBytes

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

instance SCS.GetCsrfSecret (AppContext db uh) where
  csrfSecret = appCsrfSecret . Refine.Backend.Types.csrfSecret . to (Just . SCS.CsrfSecret . cs)

instance SCS.GetSessionToken AppState where
  getSessionToken = appUserState . to (\case
    UserLoggedIn _user session -> Just . SCS.SessionToken $ userSessionText session
    UserLoggedOut              -> Nothing)
