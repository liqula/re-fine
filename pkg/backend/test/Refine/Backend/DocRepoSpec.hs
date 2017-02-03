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
    it "getChildEdits returns multiple dependencies" $ \runner -> do
      repo  <- runner $$ createRepo
      edit1 <- runner $$ createInitialEdit repo (VDocVersion "blah")
      edit2 <- runner $$ createEdit repo edit1  (VDocVersion "blah\nblah2")
      edit3 <- runner $$ createEdit repo edit1  (VDocVersion "blah\nblah3")
      edit4 <- runner $$ createEdit repo edit3  (VDocVersion "blah\nblah3\nblah4")
      edit5 <- runner $$ createEdit repo edit4  (VDocVersion "blah\nblah3\nblah4\nblah5")
      edit1deps <- runner $$ getChildEdits repo edit1
      edit2deps <- runner $$ getChildEdits repo edit2
      edit3deps <- runner $$ getChildEdits repo edit3
      edit4deps <- runner $$ getChildEdits repo edit4
      edit5deps <- runner $$ getChildEdits repo edit5

      edit1deps `shouldBe` [edit2, edit3]
      edit4deps `shouldBe` [edit5]
      edit5deps `shouldBe` []
      pendingWith "Needs more investigation in case of clonflicts."
      edit3deps `shouldBe` [edit4]
      edit2deps `shouldBe` []


cfg :: Config
cfg = Config
  { _cfgShouldMigrate = False  -- (this is ignored here)
  , _cfgShouldLog     = False  -- (this is ignored here)
  , _cfgRootDir       = "."
  , _cfgReposRoot     = "repos"
  , _cfgDBKind        = DBOnDisk "testDb"
  , _cfgPoolSize      = 5
  , _cfgFileServeRoot = Nothing
  , _cfgWarpSettings  = def
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
  createDirectoryIfMissing True $ cfg ^. cfgRootDir
  createDirectoryIfMissing True $ cfg ^. cfgReposRoot
  pure $ Nat runRepo
