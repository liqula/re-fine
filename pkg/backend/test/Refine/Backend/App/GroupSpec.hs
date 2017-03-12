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
module Refine.Backend.App.GroupSpec where

import Control.Lens
import Control.Monad (join)
import Test.Hspec

import Refine.Backend.App.Core  as App
import Refine.Backend.App.Group as App
import Refine.Backend.Database
import Refine.Backend.User
import Refine.Common.Types.Group
-- import Refine.Common.Types.Prelude
import Refine.Test.App.Runner

type AppRunner a = AppM DB UH a -> IO a

spec :: Spec
spec = do
  describe "Group" . around provideAppRunner $ do
    it "create works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.createGroup (CreateGroup "title" "desc" [] [])
        group2 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group1 `shouldBe` group2

    it "modify works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.createGroup (CreateGroup "title" "desc" [] [])
        group2 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "title1" "desc1" [] [])
        group3 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group2 `shouldBe` group3
          group1 ^. groupID `shouldBe` group2 ^. groupID
          group2 ^. groupID `shouldBe` group3 ^. groupID

    it "modify works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.createGroup (CreateGroup "title" "desc" [] [])
        group2 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "t2" "d2" [] [])
        group3 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "t3" "d3" [] [])
        group4 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group3 `shouldBe` group4
          group1 ^. groupID `shouldBe` group2 ^. groupID
          group2 ^. groupID `shouldBe` group3 ^. groupID
          group3 ^. groupID `shouldBe` group4 ^. groupID

{-
  describe "Group errorneous" . around provideAppRunner $ do
    -- provideAppRunner is not polimorphic enough
    it "non-existing group" $ \runner -> do
      (runner $ do
        !(Group _id _title _desc _parents _children) <- App.getGroup (ID 100000000)
        () <- appIO $ putStrLn "hello"
        pure ())
      `shouldThrow`
      anyException
-}
