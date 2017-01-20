{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Refine.Backend.Server
  ( startBackend
  , BackendConfig(..), defaultBackendConfig
  , Backend(..), mkBackend
  , refineApi
  ) where

import           Control.Category
import           Control.Monad.Except
import qualified Control.Natural as CN
import           Control.Natural (($$))
import           Data.String.Conversions (cs)
import           Network.Wai.Handler.Warp as Warp
import           Prelude hiding ((.), id)
import           Servant hiding (Patch)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Database (DB, DBConfig(..), DBKind(..), createDBRunner)
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.DocRepo (createRunRepo)
import Refine.Common.Rest
import Refine.Common.Types


startBackend :: IO ()
startBackend = do
  Warp.runSettings Warp.defaultSettings . backendServer =<< mkBackend defaultBackendConfig

data Backend = Backend
  { backendServer :: Application
  , backendMonad  :: App DB CN.:~> ExceptT AppError IO
  }

data BackendConfig = BackendConfig
  { backendShouldMigrate :: Bool
  , backendShouldLog     :: Bool
  , backendRootDir       :: FilePath
  }

-- | This will get better when we start taking configuration more serious.
defaultBackendConfig :: BackendConfig
defaultBackendConfig = BackendConfig True True "./.backend-data"

defaultDBConfig :: DBConfig
defaultDBConfig = DBConfig
  { _dbConfigDBKind   = DBOnDisk (backendRootDir defaultBackendConfig </> "refine.db")
  , _dbConfigPoolSize = 5
  }

mkBackend :: BackendConfig -> IO Backend
mkBackend cfg = do
  createDirectoryIfMissing True (backendRootDir defaultBackendConfig </> "docrepo")
  (runDb, userHandler) <- createDBRunner defaultDBConfig
  runDocRepo <- createRunRepo (backendRootDir defaultBackendConfig </> "docrepo/")
  let logger = Logger $ if backendShouldLog cfg then putStrLn else const $ pure ()
      app    = runApp runDb runDocRepo logger userHandler
      srv    = Servant.serve (Proxy :: Proxy RefineAPI) $ serverT app refineApi

  when (backendShouldMigrate cfg) $ do
    void $ (natThrowError . app) $$ do
      migrateDB

  pure $ Backend srv app


serverT :: (App db CN.:~> ExceptT AppError IO) -> ServerT RefineAPI (App db) -> Server RefineAPI
serverT app = enter (toServantError . cnToSn app)


toServantError :: (Monad m) => ExceptT AppError m :~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> either (throwError . fromAppError) pure)
  where
    -- FIXME: Render JSON from the errors
    -- FIXME: some (many?) of these shouldn't be err500.
    fromAppError :: AppError -> ServantErr
    fromAppError msg = err500 { errBody = cs $ show msg }


-- | The 's' prefix in the handlers stands for "server", and is used to dismabiguate between the code in
-- 'App' vs. the code in 'ServerT'.  This is slightly less noisy than qualified imports, and it's
-- (probably) only used internally in this module.
refineApi :: ServerT RefineAPI (App DB)
refineApi =
       Refine.Backend.App.listVDocs
  :<|> Refine.Backend.App.getCompositeVDoc
  :<|> Refine.Backend.App.createVDocGetComposite
  :<|> Refine.Backend.App.addComment
  :<|> sAddPatch


-- * vdocs

sAddPatch :: ID Patch -> Create Patch -> App DB Patch
sAddPatch = undefined
