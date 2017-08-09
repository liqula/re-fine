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
  , Backend(..), mkProdBackend
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
import qualified Web.Users.Types as Users

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
    DBInMemory    -> pure ()
    DBOnDisk path -> do
      cpath <- canonicalizePath path
      createDirectoryIfMissing True (dropFileName cpath)


-- * backend creation

data Backend db = Backend
  { backendServer :: Application
  , backendRunApp :: AppM db P.:~> ExceptT AppError IO
  }

refineApi :: (Database db) => ServerT RefineAPI (AppM db)
refineApi =
       App.getCompositeVDocOnHead
  :<|> App.getVDoc
  :<|> App.createVDocGetComposite
  :<|> App.updateVDoc
  :<|> App.addEdit
  :<|> App.getEdit
  :<|> App.mergeEdit
  :<|> App.updateEdit
  :<|> App.addNote
  :<|> App.getNote
  :<|> App.addQuestion
  :<|> App.addAnswer
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
  :<|> App.putSimpleVoteOnEdit
  :<|> App.deleteSimpleVoteOnEdit
  :<|> App.getSimpleVotesOnEdit


startBackend :: Config -> IO ()
startBackend cfg = do
  Warp.runSettings (warpSettings cfg) . backendServer =<< mkProdBackend cfg

runCliAppCommand :: Config -> AppM DB a -> IO ()
runCliAppCommand cfg cmd = do
  backend <- mkProdBackend cfg
  void $ (natThrowError . backendRunApp backend) $$ cmd

mkProdBackend :: Config -> IO (Backend DB)
mkProdBackend cfg = mkBackend cfg $ do
  () <- migrateDB cfg
  gs <- App.getGroups
  when (null gs) $ do
    void . addGroup $ CreateGroup "default" "default group" [] []
  pure ()

mkBackend :: Config -> AppM DB a -> IO (Backend DB)
mkBackend cfg migrate = do
  createDataDirectories cfg

  -- create runners
  (dbRunner, dbNat) <- createDBNat cfg
  backend <- mkServerApp cfg dbNat dbRunner

  -- migration
  result <- runExceptT (backendRunApp backend $$ migrate)
  either (error . show) (const $ pure ()) result

  pure backend


mkServerApp :: Database db => Config -> MkDBNat db -> DBRunner -> IO (Backend db)
mkServerApp cfg dbNat dbRunner = do
  let cookie = SCS.def { SCS.setCookieName = refineCookieName, SCS.setCookiePath = Just "/" }
      logger = Logger $ case cfg ^. cfgLogger of
        LogCfgFile file -> appendFile file . (<> "\n")
        LogCfgStdOut    -> putStrLn
        LogCfgDevNull   -> const $ pure ()
      app    = runApp dbNat
                      dbRunner
                      logger
                      cfg

  -- FIXME: Static content delivery is not protected by "Servant.Cookie.Session" To achive that, we
  -- may need to refactor, e.g. by using extra arguments in the end point types.
  srvApp <- serveAction
              (Proxy :: Proxy RefineAPI)
              (Proxy :: Proxy AppState)
              cookie
              (Nat liftIO)
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
  AppMergeError base e1 e2 s -> ApiMergeError $ cs (show (base, e1, e2)) <> ": " <> s
  AppRebaseError{}       -> ApiRebaseError
  AppSmtpError{}         -> ApiSmtpError

-- | so we don't have to export backend types to the frontend.
createUserErrorToApiError :: Users.CreateUserError -> ApiErrorCreateUser
createUserErrorToApiError Users.InvalidPassword              = ApiErrorInvalidPassword
createUserErrorToApiError Users.UsernameAlreadyTaken         = ApiErrorUsernameAlreadyTaken
createUserErrorToApiError Users.EmailAlreadyTaken            = ApiErrorEmailAlreadyTaken
createUserErrorToApiError Users.UsernameAndEmailAlreadyTaken = ApiErrorUsernameAndEmailAlreadyTaken

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
  AppRebaseError{}         -> err500
  AppSmtpError{}           -> err500

dbServantErr :: DBError -> ServantErr
dbServantErr = \case
  DBUnknownError    _ -> err500
  DBNotFound        _ -> err404
  DBNotUnique       _ -> err409
  DBException       _ -> err500
  DBUserNotLoggedIn   -> err403
  DBMigrationParseErrors _ -> err500
  DBUnsafeMigration _      -> err500

userCreationError :: Users.CreateUserError -> ServantErr
userCreationError = \case
  Users.InvalidPassword              -> err400
  Users.UsernameAlreadyTaken         -> err409
  Users.EmailAlreadyTaken            -> err409
  Users.UsernameAndEmailAlreadyTaken -> err409

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
