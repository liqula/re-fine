{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.App.GroupSpec where
#include "import_backend.hs"

import Test.Hspec

import Refine.Backend.App.Core  as App
import Refine.Backend.App.Group as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Common.Types
import Refine.Common.Types.Prelude (ID(..))


type AppRunner a = AppM DB a -> ExceptT ApiError IO a

spec :: Spec
spec = do
  describe "Group" . around provideAppRunner $ do
    it "create works" $ \(runner :: AppRunner (IO ())) -> do
      join . throwApiErrors . runner $ do
        group1 <- App.addGroup (CreateGroup "title" "desc" [] [] mempty Nothing)
        group2 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group1 `shouldBe` group2

    it "modify once works" $ \(runner :: AppRunner (IO ())) -> do
      join . throwApiErrors . runner $ do
        group1 <- App.addGroup (CreateGroup "title" "desc" [] [] mempty Nothing)
        group2 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "title1" "desc1" [] [] mempty Nothing)
        group3 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group1 `shouldNotBe` group3
          group2 `shouldBe` group3
          group1 ^. groupID `shouldBe` group2 ^. groupID
          group2 ^. groupID `shouldBe` group3 ^. groupID

    it "modify twice works" $ \(runner :: AppRunner (IO ())) -> do
      join . throwApiErrors . runner $ do
        group1 <- App.addGroup (CreateGroup "title" "desc" [] [] mempty Nothing)
        group2 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "t2" "d2" [] [] mempty Nothing)
        group3 <- App.modifyGroup (group1 ^. groupID) (CreateGroup "t3" "d3" [] [] mempty Nothing)
        group4 <- App.getGroup (group1 ^. groupID)
        pure $ do
          group1 `shouldNotBe` group4
          group2 `shouldNotBe` group4
          group3 `shouldBe` group4
          group1 ^. groupID `shouldBe` group2 ^. groupID
          group2 ^. groupID `shouldBe` group3 ^. groupID
          group3 ^. groupID `shouldBe` group4 ^. groupID

    it "create with parents and children works" $ \(runner :: AppRunner (IO ())) -> do
      join . throwApiErrors . runner $ do
        group1 <- App.addGroup (CreateGroup "t1" "d1" [] [] mempty Nothing)
        group2 <- App.addGroup (CreateGroup "t2" "d2" [] [] mempty Nothing)
        group3 <- App.addGroup (CreateGroup "t3" "d3" [group1 ^. groupID] [group2 ^. groupID] mempty Nothing)
        group4 <- App.getGroup (group3 ^. groupID)
        pure $ do
          group3 `shouldBe` group4
          group4 ^. groupParents  `shouldBe` [group1 ^. groupID]
          group4 ^. groupChildren `shouldBe` [group2 ^. groupID]

    it "modify changes subgroups" $ \(runner :: AppRunner (IO ())) ->
      join . throwApiErrors . runner $ do
        group1  <- App.addGroup (CreateGroup "t1" "d1" [] [] mempty Nothing)
        group2  <- App.addGroup (CreateGroup "t2" "d2" [] [] mempty Nothing)
        group3  <- App.addGroup (CreateGroup "t3" "d3" [] [] mempty Nothing)
        group3' <- App.modifyGroup (group3 ^. groupID) (CreateGroup "t3" "d3" [group1 ^. groupID] [group2 ^. groupID] mempty Nothing)
        group4  <- App.getGroup (group3' ^. groupID)
        pure $ do
          group3' `shouldBe` group4
          group4 ^. groupParents     `shouldBe` [group1 ^. groupID]
          group4 ^. groupChildren    `shouldBe` [group2 ^. groupID]

    it "add and remove subgroup" $ \(runner :: AppRunner (IO ())) ->
      join . throwApiErrors . runner $ do
        parentg1 <- App.addGroup (CreateGroup "title" "desc" [] [] mempty Nothing)
        childg1  <- App.addGroup (CreateGroup "title2" "desc2" [] [] mempty Nothing)
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

  describe "error handling" . around provideAppRunner $ do
    -- The way polymorphism works, we have to open a new 'around' here, or the 'provideAppRunner'
    -- will be restricted to an inconsistent type.

    it "remove group" $ \runner -> do
      (throwApiErrors . runner $ do
          group <- App.addGroup (CreateGroup "title" "desc" [] [] mempty Nothing)
          ()    <- App.removeGroup (group ^. groupID)
          void $ App.getGroup (group ^. groupID))
       `shouldThrow`
       anyException

    it "non-existing group" $ \runner -> do
      (throwApiErrors . runner $ do
          (Group _gid _title _desc _parents _children _ _ _) <- App.getGroup (ID 100000000)
          pure ())
       `shouldThrow`
       anyException
