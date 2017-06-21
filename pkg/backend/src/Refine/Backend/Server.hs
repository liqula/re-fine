{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
  , runCliAppCommand
  , Backend(..), mkProdBackend, mkDevModeBackend
  , refineApi
  ) where

import Refine.Backend.Prelude as P

import           Debug.Trace (trace)  -- (please keep this until we have better logging)
import           Network.Wai.Handler.Warp as Warp
import qualified Servant.Cookie.Session as SCS
import           Servant.Cookie.Session (serveAction)
import           Servant.Server.Internal (responseServantErr)
import qualified Servant.Utils.Enter
import           Servant.Utils.StaticFiles (serveDirectory)
import           System.Directory (canonicalizePath, createDirectoryIfMissing)
import           System.FilePath (dropFileName)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB (migrateDB)
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Types
import Refine.Backend.User
import Refine.Common.Allow
import Refine.Common.Rest
import Refine.Common.Types.Core (Edit)


-- * Constants

refineCookieName :: SBS
refineCookieName = "refine"


-- * Initialization

createDataDirectories :: Config -> IO ()
createDataDirectories cfg = do
  case cfg ^. cfgDBKind of
    DBInMemory    -> pure ()
    DBOnDisk path -> do
      cpath <- canonicalizePath path
      createDirectoryIfMissing True (dropFileName cpath)


-- * backend creation

data Backend db uh = Backend
  { backendServer :: Application
  , backendRunApp :: AppM db uh P.:~> ExceptT AppError IO
  }

type MonadRefine db uh =
  ( MonadApp db uh
  , Allow (ProcessPayload Edit) Edit
  )

refineApi :: MonadRefine db uh => ServerT RefineAPI (AppM db uh)
refineApi =
       Refine.Backend.App.listVDocs
  :<|> Refine.Backend.App.getCompositeVDocOnHead
  :<|> Refine.Backend.App.createVDocGetComposite
  :<|> Refine.Backend.App.addEdit
  :<|> Refine.Backend.App.addNote
  :<|> Refine.Backend.App.addQuestion
  :<|> Refine.Backend.App.addAnswer
  :<|> Refine.Backend.App.addDiscussion
  :<|> Refine.Backend.App.addStatement
  :<|> Refine.Backend.App.createUser
  :<|> Refine.Backend.App.login
  :<|> Refine.Backend.App.logout
  :<|> Refine.Backend.App.getTranslations
  :<|> Refine.Backend.App.addGroup
  :<|> Refine.Backend.App.changeSubGroup
  :<|> Refine.Backend.App.changeRole
  :<|> Refine.Backend.App.addProcess
  :<|> Refine.Backend.App.changeProcess
  :<|> Refine.Backend.App.removeProcess
  :<|> Refine.Backend.App.putSimpleVoteOnEdit
  :<|> Refine.Backend.App.deleteSimpleVoteOnEdit
  :<|> Refine.Backend.App.getSimpleVotesOnEdit


startBackend :: Config -> IO ()
startBackend cfg =
  if cfg ^. cfgDevMode
    then do backend <- mkDevModeBackend cfg mockLogin
            Warp.runSettings (warpSettings cfg) $ backendServer backend
    else do backend <- mkProdBackend cfg
            Warp.runSettings (warpSettings cfg) $ backendServer backend

runCliAppCommand :: Config -> AppM DB UH a -> IO ()
runCliAppCommand cfg cmd = do
  backend <- mkProdBackend cfg
  void $ (natThrowError . backendRunApp backend) $$ cmd

mkProdBackend :: Config -> IO (Backend DB UH)
mkProdBackend cfg = mkBackend cfg uhNat (migrateDB cfg)

mkDevModeBackend :: Config -> MockUH_ -> IO (Backend DB FreeUH)
mkDevModeBackend cfg mock = mkBackend cfg (\_ -> uhNat mock) (migrateDB cfg)

mkBackend :: MonadUserHandle uh => Config -> (UserDB -> UHNat uh) -> AppM DB uh a -> IO (Backend DB uh)
mkBackend cfg initUH migrate = do
  createDataDirectories cfg

  -- create runners
  (dbRunner, dbNat, runUserHandle) <- createDBNat cfg
  backend    <- mkServerApp cfg dbNat dbRunner (initUH runUserHandle)

  -- migration
  result <- runExceptT (backendRunApp backend $$ migrate)
  case result of
    Left err -> error $ show err
    Right _  -> pure ()

  pure backend


mkServerApp
    :: MonadRefine db uh
    => Config -> MkDBNat db -> DBRunner -> UHNat uh -> IO (Backend db uh)
mkServerApp cfg dbNat dbRunner uh = do
  poFilesRoot <- cfg ^. cfgPoFilesRoot . to canonicalizePath
  let cookie = SCS.def { SCS.setCookieName = refineCookieName, SCS.setCookiePath = Just "/" }
      logger = Logger $ if cfg ^. cfgShouldLog then putStrLn else const $ pure ()
      app    = runApp dbNat
                      dbRunner
                      uh
                      logger
                      (cfg ^. cfgCsrfSecret . to CsrfSecret)
                      (cfg ^. cfgSessionLength)
                      poFilesRoot
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
              (Just (P.serve (Proxy :: Proxy Raw) (maybeServeDirectory (cfg ^. cfgFileServeRoot))))

  pure $ Backend srvApp app

maybeServeDirectory :: Maybe FilePath -> Server Raw
maybeServeDirectory = maybe (\_ respond -> respond $ responseServantErr err404) serveDirectory

{-# ANN toServantError ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
toServantError :: (Monad m) => ExceptT AppError m Servant.Utils.Enter.:~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> leftToError fromAppError)
  where
    -- FIXME: some (many?) of these shouldn't be err500.
    -- FIXME: implement better logging.
    fromAppError :: AppError -> ServantErr
    fromAppError err = traceShow' err $ (appServantErr err) { errBody = encode $ toApiError err }

    traceShow' :: (Show a) => a -> b -> b
    traceShow' a = trace ("toServantError: " <> show a)

-- FIXME: review this; is this really needed?
toApiError :: AppError -> ApiError
toApiError = \case
  AppUnknownError e      -> ApiUnknownError e
  AppVDocVersionError    -> ApiVDocVersionError
  AppDBError e           -> ApiDBError . cs $ show e
  AppUserNotFound e      -> ApiUserNotFound e
  AppUserNotLoggedIn     -> ApiUserNotLoggedIn
  AppUserCreationError e -> ApiUserCreationError $ createUserErrorToApiError e
  AppCsrfError e         -> ApiCsrfError e
  AppSessionError        -> ApiSessionError
  AppSanityCheckError e  -> ApiSanityCheckError e
  AppUserHandleError e   -> ApiUserHandleError . cs $ show e
  AppL10ParseErrors e    -> ApiL10ParseErrors e
  AppUnauthorized        -> ApiUnauthorized
  AppMergeError{}        -> ApiMergeError

-- | so we don't have to export backend types to the frontend.
createUserErrorToApiError :: CreateUserError -> ApiErrorCreateUser
createUserErrorToApiError InvalidPassword              = ApiErrorInvalidPassword
createUserErrorToApiError UsernameAlreadyTaken         = ApiErrorUsernameAlreadyTaken
createUserErrorToApiError EmailAlreadyTaken            = ApiErrorEmailAlreadyTaken
createUserErrorToApiError UsernameAndEmailAlreadyTaken = ApiErrorUsernameAndEmailAlreadyTaken

-- | Turns AppError to its kind of servant error.
appServantErr :: AppError -> ServantErr
appServantErr = \case
  AppUnknownError _        -> err500
  AppVDocVersionError      -> err409
  AppDBError dbe           -> dbServantErr dbe
  AppUserNotFound _        -> err404
  AppUserNotLoggedIn       -> err403
  AppUserCreationError uce -> userCreationError uce
  AppCsrfError _           -> err403
  AppSessionError          -> err403
  AppSanityCheckError _    -> err409
  AppUserHandleError _     -> err500
  AppL10ParseErrors _      -> err500
  AppUnauthorized          -> err403
  AppMergeError{}          -> err500

dbServantErr :: DBError -> ServantErr
dbServantErr = \case
  DBUnknownError    _ -> err500
  DBNotFound        _ -> err404
  DBNotUnique       _ -> err409
  DBException       _ -> err500
  DBUserNotLoggedIn   -> err403
  DBMigrationParseErrors _ -> err500
  DBUnsafeMigration _      -> err500

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
