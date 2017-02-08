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
  ( startBackend
  , Backend(..), mkBackend
  , refineApi
  ) where

import           Control.Category
import           Control.Lens ((^.), iso, prism', to)
import           Control.Monad.Except
import qualified Control.Natural as CN
import           Control.Natural (($$))
import           Data.Aeson (encode)
import           Data.String.Conversions (cs)
import           Debug.Trace (traceShow)  -- (please keep this until we have better logging)
import           Network.Wai.Handler.Warp as Warp
import           Prelude hiding ((.), id)
import           Servant
import           Servant.Server.Internal (responseServantErr)
import           Servant.Utils.StaticFiles (serveDirectory)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (dropFileName)

import Servant.Cookie.Session
import qualified Servant.Cookie.Session.CSRF as SCS (CsrfSecret(..), CsrfToken(..))
import Servant.Cookie.Session.CSRF hiding (CsrfSecret(..), CsrfToken(..))
import Servant.Cookie.Session.Types
import Web.Cookie
import Crypto.Random (MonadRandom(..))
import Servant.Missing (ThrowError500(..))

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database (DB, createDBRunner)
import Refine.Backend.DocRepo (createRunRepo)
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Types
import Refine.Common.Rest
import Refine.Prelude (monadError)


startBackend :: Config -> IO ()
startBackend cfg = do
  Warp.runSettings (warpSettings cfg) . backendServer =<< mkBackend cfg

data Backend = Backend
  { backendServer :: Application
  , backendMonad  :: App DB CN.:~> ExceptT AppError IO
  }

mkBackend :: Config -> IO Backend
mkBackend cfg = do
  createDataDirectories cfg
  (runDb, userHandler) <- createDBRunner cfg
  runDocRepo <- createRunRepo cfg
  let refineCookie = def { setCookieName = "refine", setCookiePath = Just "/" }
      logger = Logger $ if cfg ^. cfgShouldLog then putStrLn else const $ pure ()
      app    = runApp runDb runDocRepo logger userHandler (cfg ^. cfgCsrfSecret . to CsrfSecret)
  srvApp <- serveAction
              (Proxy :: Proxy RefineAPI)
              (Proxy :: Proxy AppState)
              refineCookie
              (Nat appIO)
              (toServantError . cnToSn app)
              refineApi

  let srv = Servant.serve (Proxy :: Proxy (Raw :<|> "static" :> Raw)) $
              srvApp :<|> maybeServeDirectory (cfg ^. cfgFileServeRoot)

  when (cfg ^. cfgShouldMigrate) $ do
    void $ (natThrowError . app) $$ do
      migrateDB

  pure $ Backend srv app

instance MonadRandom (App db) where
  getRandomBytes = appIO . getRandomBytes

instance ThrowError500 AppError where
  error500 = prism' toAppError fromAppError
    where
      toAppError :: String -> AppError
      toAppError = AppCsrfError . cs

      fromAppError :: AppError -> Maybe String
      fromAppError (AppCsrfError e) = Just $ cs e
      fromAppError _                = Nothing

instance HasSessionCsrfToken AppState where
  sessionCsrfToken = appCsrfToken . csrfTokenIso
    where
      fromSCS = CsrfToken . cs . SCS.fromCsrfToken
      toSCS   = SCS.CsrfToken . cs . _csrfToken
      csrfTokenIso = iso (fmap toSCS) (fmap fromSCS)

instance GetCsrfSecret (AppContext db) where
  csrfSecret = appCsrfSecret . Refine.Backend.Types.csrfSecret . to (Just . SCS.CsrfSecret . cs)

instance GetSessionToken AppState where
  getSessionToken = appUserState . to (\case
    ActiveUser    us -> Just . SessionToken . userSessionText $ us
    NonActiveUser    -> Nothing)

createDataDirectories :: Config -> IO ()
createDataDirectories cfg = do
  createDirectoryIfMissing True (cfg ^. cfgReposRoot)
  case cfg ^. cfgDBKind of
    DBInMemory    -> pure ()
    DBOnDisk path -> createDirectoryIfMissing True (dropFileName path)

toServantError :: (Monad m) => ExceptT AppError m :~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> monadError fromAppError)
  where
    -- FIXME: some (many?) of these shouldn't be err500.
    -- FIXME: implement better logging.
    fromAppError :: AppError -> ServantErr
    fromAppError msg = traceShow msg $ err500 { errBody = encode msg }

-- | The 's' prefix in the handlers stands for "server", and is used to dismabiguate between the code in
-- 'App' vs. the code in 'ServerT'.  This is slightly less noisy than qualified imports, and it's
-- (probably) only used internally in this module.
refineApi :: ServerT RefineAPI (App DB)
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

maybeServeDirectory :: Maybe FilePath -> Server Raw
maybeServeDirectory = maybe (\_ respond -> respond $ responseServantErr err404) serveDirectory
