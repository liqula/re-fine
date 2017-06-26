{-# LANGUAGE NoImplicitPrelude          #-}
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

module Refine.Backend.Test.AppRunner where

import Refine.Backend.Prelude

import Test.Hspec
import Test.QuickCheck
import Control.Exception.Base (evaluate)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Server
import Refine.Backend.Test.Util
import Refine.Backend.Types
import Refine.Backend.User


provideAppRunner :: ActionWith (AppM DB UH a -> IO a) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, testDb) <- createAppRunner
  action runner
  removeFile testDb

createAppRunner :: forall a . IO (AppM DB UH a -> IO a, FilePath)
createAppRunner = do
  let testDb    = "test.db"
      poRoot    = "./repos" -- FIXME: Change this when needed. Not used at the moment.

      cfg = Config
        { _cfgShouldLog     = False  -- (this is ignored here)
        , _cfgDBKind        = DBOnDisk testDb
        , _cfgPoolSize      = 5
        , _cfgFileServeRoot = Nothing
        , _cfgWarpSettings  = def
        , _cfgCsrfSecret    = "CSRF-SECRET"
        , _cfgSessionLength = TimespanSecs 30
        , _cfgDevMode       = False
        , _cfgPoFilesRoot   = poRoot
        }

  (dbRunner, dbNat, userHandler) <- createDBNat cfg
  let logger = Logger . const $ pure ()
      runner :: forall b . AppM DB UH b -> IO b
      runner m = ((natThrowError . runApp
                                    dbNat
                                    dbRunner
                                    (uhNat userHandler)
                                    logger
                                    (cfg ^. cfgCsrfSecret . to CsrfSecret)
                                    (cfg ^. cfgSessionLength)
                                    poRoot
                                    id) $$ m) >>= evaluate -- without evaluate we have issue #389

  void $ runner (migrateDB cfg >> initializeDB)
  pure (runner, testDb)

monadicApp :: (AppM DB UH Property -> IO Property) -> AppM DB UH Property -> Property
monadicApp p = ioProperty . p

errorNat :: (Show e, Functor m) => ExceptT e m :~> m
errorNat = NT (fmap (either (error . show) id) . runExceptT)

provideDevModeAppRunner :: ActionWith (AppM DB FreeUH a -> IO a) -> IO ()
provideDevModeAppRunner action = withTempCurrentDirectory $ do
  backend <- backendRunApp <$> mkDevModeBackend (def & cfgShouldLog .~ False) mockLogin
  void $ action (unwrapNT (errorNat . backend))
