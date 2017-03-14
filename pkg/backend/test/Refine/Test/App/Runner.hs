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
module Refine.Test.App.Runner where

import Control.Category
import Control.Lens
import Control.Monad (void)
import Control.Monad.Except
import Control.Natural
import Data.Default
import Prelude hiding ((.), id)
import System.Directory
import Test.Hspec
import Test.QuickCheck

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Server
import Refine.Backend.Test.Util
import Refine.Backend.Types
import Refine.Backend.User
import Refine.Prelude


provideAppRunner :: ActionWith (AppM DB UH a -> IO a) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, testDb, reposRoot) <- createAppRunner
  action runner
  removeFile testDb
  removeDirectoryRecursive reposRoot

createAppRunner :: forall a . IO (AppM DB UH a -> IO a, FilePath, FilePath)
createAppRunner = do
  let testDb    = "test.db"
      reposRoot = "./repos"
      poRoot    = "./repos" -- FIXME: Change this when needed. Not used at the moment.

      cfg = Config
        { _cfgShouldMigrate = False  -- (this is ignored here)
        , _cfgShouldLog     = False  -- (this is ignored here)
        , _cfgReposRoot     = reposRoot
        , _cfgDBKind        = DBOnDisk testDb
        , _cfgPoolSize      = 5
        , _cfgFileServeRoot = Nothing
        , _cfgWarpSettings  = def
        , _cfgCsrfSecret    = "CSRF-SECRET"
        , _cfgSessionLength = TimespanSecs 30
        , _cfgDevMode       = False
        , _cfgPoFilesRoot   = poRoot
        }

  createDirectoryIfMissing True $ cfg ^. cfgReposRoot
  (dbNat, userHandler) <- createDBNat cfg
  drepoNat <- createRepoNat cfg
  let logger = Logger . const $ pure ()
      runner :: forall b . AppM DB UH b -> IO b
      runner m = (natThrowError . runApp
                                    dbNat
                                    drepoNat
                                    (uhNat userHandler)
                                    logger
                                    (cfg ^. cfgCsrfSecret . to CsrfSecret)
                                    (cfg ^. cfgSessionLength)
                                    poRoot
                                    id) $$ m

  void $ runner migrateDB
  pure (runner, testDb, reposRoot)

monadicApp :: (AppM DB UH Property -> IO Property) -> AppM DB UH Property -> Property
monadicApp p = ioProperty . p

errorNat :: (Show e, Functor m) => ExceptT e m :~> m
errorNat = Nat (fmap (either (error . show) id) . runExceptT)

provideDevModeAppRunner :: ActionWith (AppM DB FreeUH a -> IO a) -> IO ()
provideDevModeAppRunner action = withTempCurrentDirectory $ do
  backend <- backendRunApp <$> mkDevModeBackend (def & cfgShouldLog .~ False) mockLogin
  void $ action (run (errorNat . backend))
