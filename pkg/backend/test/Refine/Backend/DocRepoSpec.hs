{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.DocRepoSpec where

import Control.Natural
import Control.Lens ((^.))
import Data.Default
import System.Directory
import Test.Hspec

import Refine.Backend.Config
import Refine.Backend.DocRepo
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Common.Types.VDoc



spec :: Spec
spec = do
  describe "DocRepo" . around provideRunner $ do
    it "getVersion returns a version" $ \runner -> do
      repo  <- runner $$ createRepo
      edit1 <- runner $$ createInitialEdit repo (vdocVersionFromST "blah")
      (runner $$ getVersion repo edit1) `shouldReturn` vdocVersionFromST "blah"

    it "getChildEdits returns multiple dependencies" $ \runner -> do
      repo  <- runner $$ createRepo
      edit1 <- runner $$ createInitialEdit repo (vdocVersionFromST "blah")
      edit2 <- runner $$ createEdit repo edit1  (vdocVersionFromST "blah\nblah2")
      edit3 <- runner $$ createEdit repo edit1  (vdocVersionFromST "blah\nblah3")
      edit4 <- runner $$ createEdit repo edit3  (vdocVersionFromST "blah\nblah3\nblah4")
      edit5 <- runner $$ createEdit repo edit4  (vdocVersionFromST "blah\nblah3\nblah4\nblah5")
      edit1children <- runner $$ getChildEdits repo edit1
      edit2children <- runner $$ getChildEdits repo edit2
      edit3children <- runner $$ getChildEdits repo edit3
      edit4children <- runner $$ getChildEdits repo edit4
      edit5children <- runner $$ getChildEdits repo edit5

      edit1children `shouldBe` [edit2, edit3]
      edit4children `shouldBe` [edit5]
      edit5children `shouldBe` []
      pendingWith "Needs more investigation in case of conflicts."
      edit3children `shouldBe` [edit4]
      edit2children `shouldBe` []


cfg :: Config
cfg = Config
  { _cfgShouldMigrate = False  -- (this is ignored here)
  , _cfgShouldLog     = False  -- (this is ignored here)
  , _cfgReposRoot     = "./repos"
  , _cfgDBKind        = DBOnDisk "./testDb"
  , _cfgPoolSize      = 5
  , _cfgFileServeRoot = Nothing
  , _cfgWarpSettings  = def
  , _cfgCsrfSecret    = "CSRF-SECRET"
  }

provideRunner :: ActionWith (DocRepo :~> IO) -> IO ()
provideRunner action = withTempCurrentDirectory $ do
  runner <- createDocRepoRunner
  action runner
  removeDirectoryRecursive (cfg ^. cfgReposRoot)

runRepo :: DocRepo a -> IO a
runRepo = fmap (either (error . show) id) . runDocRepo cfg

createDocRepoRunner :: IO (DocRepo :~> IO)
createDocRepoRunner = do
  createDirectoryIfMissing True $ cfg ^. cfgReposRoot
  pure $ Nat runRepo
