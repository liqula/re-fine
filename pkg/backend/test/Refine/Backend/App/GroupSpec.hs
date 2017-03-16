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
import Control.Monad (join, void)
import Test.Hspec

import Refine.Backend.App.Core  as App
import Refine.Backend.App.Group as App
import Refine.Backend.Database
import Refine.Backend.Test.Util (forceEval)
import Refine.Backend.User
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude (ID(..))
import Refine.Test.App.Runner


type AppRunner a = AppM DB UH a -> IO a

spec :: Spec
spec = do
  describe "Group" . around provideAppRunner $ do
    it "create works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.addGroup (CreateGroup "title" "desc" [] [])
        group2 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group1 `shouldBe` group2

    it "modify works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.addGroup (CreateGroup "title" "desc" [] [])
        group2 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "title1" "desc1" [] [])
        group3 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group2 `shouldBe` group3
          group1 ^. groupID `shouldBe` group2 ^. groupID
          group2 ^. groupID `shouldBe` group3 ^. groupID

    it "modify works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.addGroup (CreateGroup "title" "desc" [] [])
        group2 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "t2" "d2" [] [])
        group3 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "t3" "d3" [] [])
        group4 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group3 `shouldBe` group4
          group1 ^. groupID `shouldBe` group2 ^. groupID
          group2 ^. groupID `shouldBe` group3 ^. groupID
          group3 ^. groupID `shouldBe` group4 ^. groupID

    it "create with parents and children works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        group1 <- App.addGroup (CreateGroup "t1" "d1" [] [])
        group2 <- App.addGroup (CreateGroup "t2" "d2" [] [])
        group3 <- App.addGroup (CreateGroup "t3" "d3" [group1 ^. groupID] [group2 ^. groupID])
        group4 <- App.getGroup (group3 ^. groupID)
        pure $ do
          group4 ^. groupParents  `shouldBe` [group1 ^. groupID]
          group4 ^. groupChildren `shouldBe` [group2 ^. groupID]

    it "modify changes subgroups" $ \(runner :: AppRunner (IO ())) ->
      join . runner $ do
        group1  <- App.addGroup (CreateGroup "t1" "d1" [] [])
        group2  <- App.addGroup (CreateGroup "t2" "d2" [] [])
        group3  <- App.addGroup (CreateGroup "t3" "d3" [] [])
        group3' <- App.modifyGroup (group3 ^. groupID) (CreateGroup "t3" "d3" [group1 ^. groupID] [group2 ^. groupID])
        group4  <- App.getGroup (group3' ^. groupID)
        pure $ do
          group4 ^. groupParents     `shouldBe` [group1 ^. groupID]
          group4 ^. groupChildren    `shouldBe` [group2 ^. groupID]

    it "add and remove subgroup" $ \(runner :: AppRunner (IO ())) ->
      join . runner $ do
        parentg1 <- App.addGroup (CreateGroup "title" "desc" [] [])
        childg1  <- App.addGroup (CreateGroup "title2" "desc2" [] [])
        ()       <- App.addSubGroup (parentg1 ^. groupID) (childg1 ^. groupID)
        parentg2 <- App.getGroup (parentg1 ^. groupID)
        childg2  <- App.getGroup (childg1 ^. groupID)
        ()       <- App.removeSubGroup (parentg1 ^. groupID) (childg1 ^. groupID)
        parentg3 <- App.getGroup (parentg1 ^. groupID)
        childg3  <- App.getGroup (childg1 ^. groupID)
        pure $ do
          parentg2 ^. groupParents  `shouldBe` []
          parentg2 ^. groupChildren `shouldBe` [childg1 ^. groupID]
          childg2  ^. groupParents  `shouldBe` [parentg1 ^. groupID]
          childg2  ^. groupChildren `shouldBe` []

          parentg3 ^. groupParents  `shouldBe` []
          parentg3 ^. groupChildren `shouldBe` []
          childg3  ^. groupParents  `shouldBe` []
          childg3  ^. groupChildren `shouldBe` []

  describe "Group errorneous" . around provideAppRunner $ do
    -- provideAppRunner is not polimorphic enough

    it "remove group" $ \runner -> do
      (forceEval . runner $ do
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          ()    <- App.removeGroup (group ^. groupID)
          void $ App.getGroup (group ^. groupID))
       `shouldThrow`
       anyException

    it "non-existing group" $ \runner -> do
      (forceEval . runner $ do
          (Group _gid _title _desc _parents _children) <- App.getGroup (ID 100000000)
          pure ())
       `shouldThrow`
       anyException
