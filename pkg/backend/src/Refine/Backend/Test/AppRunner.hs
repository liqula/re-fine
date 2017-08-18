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
import Refine.Backend.Test.Util
import Refine.Common.Types


-- | App actions run in god mode.  See 'unsafeBeAGod'.
provideAppRunner :: ActionWith (AppM DB a -> IO a) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, destroy) <- createAppRunner
  action runner
  destroy

-- | App actions run in god mode.  See 'unsafeBeAGod'.
createAppRunner :: forall a . IO (AppM DB a -> IO a, IO ())
createAppRunner = do
  let dbFilePath = "./test.db"
      cfg = Config
        { _cfgLogger        = LogCfg LogCfgDevNull LogWarning
        , _cfgDBKind        = DBOnDisk dbFilePath
        , _cfgPoolSize      = 5
        , _cfgFileServeRoot = Nothing
        , _cfgWarpSettings  = def
        , _cfgCsrfSecret    = "CSRF-SECRET"
        , _cfgSessionLength = TimespanSecs 30
        , _cfgPoFilesRoot   = "."
        , _cfgSmtp          = Nothing
        , _cfgClient        = def
        , _cfgWSPingPeriod  = TimespanSecs 1
        , _cfgAllAreGods    = True
        }

  (dbRunner, dbNat, destroy) <- createDBNat cfg
  let guardWithGodhood = if cfg ^. cfgAllAreGods then (unsafeBeAGod >>) else id
      logger = Logger . const $ pure ()
      runner :: forall b . AppM DB b -> IO b
      runner m = ((natThrowError . runApp
                                    dbNat
                                    dbRunner
                                    logger
                                    cfg) $$ guardWithGodhood m) >>= evaluate -- without evaluate we have issue #389

  void . runner $ do
    migrateDB cfg
    initializeDB [CliCreateGroup $ CreateGroup "Universe" "The group that contains everything" [] []]
  pure (runner, destroy)

monadicApp :: (AppM DB Property -> IO Property) -> AppM DB Property -> Property
monadicApp p = ioProperty . p
