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
module Refine.Backend.App.RoleSpec where

import Refine.Backend.Prelude

import Test.Hspec
import Test.QuickCheck

import Refine.Backend.App.Core   as App
import Refine.Backend.App.Group  as App
import Refine.Backend.App.Role   as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types
import Refine.Common.Types.Prelude (ID(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


type RoleAppRunner = AppM DB (IO ()) -> IO (IO ())

spec :: Spec
spec = around provideAppRunner $ do
    describe "assign role" $ do
      context "role was not previously assigned" $ do
        it "adds role" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = GroupMember
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            -- WHEN
            ()    <- App.assignGroupRole role user (group ^. groupID)
            -- THEN
            roles <- App.allGroupRolesIn user (group ^. groupID)
            pure $ do
              -- outcome: new role is assigned
              roles `shouldBe` [role]

      context "role was previously assigned" $ do
        it "does nothing" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = GroupMember
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            -- WHEN
            () <- App.assignGroupRole role user (group ^. groupID)
            () <- App.assignGroupRole role user (group ^. groupID)
            -- THEN
            roles <- App.allGroupRolesIn user (group ^. groupID)
            pure $ do
              -- outcome: the role stays
              roles `shouldBe` [role]

      it "role is always in new role set, old and new role are equal except for assigned role" $
        \(runner :: RoleAppRunner) -> do
          property $ \(role :: GroupRole, roles :: [GroupRole]) -> do
            join . runner $ do
              let uid = ID 1
              group       <- App.addGroup (CreateGroup "title" "desc" [] [])
              ()          <- forM_ roles $ \r -> App.assignGroupRole r uid (group ^. groupID)
              rolesBefore <- App.allGroupRolesIn uid (group ^. groupID)
              ()          <- App.assignGroupRole role uid (group ^. groupID)
              rolesAfter  <- App.allGroupRolesIn uid (group ^. groupID)
              pure $ do
                sort rolesBefore `shouldBe` nub (sort roles)
                rolesAfter `shouldContain` [role]
                sort (rolesBefore \\ [role]) `shouldBe` sort (rolesAfter \\ [role])

    describe "unassign role" $ do
      context "role previously assigned" $ do
        it "unassigns the given role" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = GroupMember
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            ()    <- App.assignGroupRole role user (group ^. groupID)
            -- WHEN
            ()    <- App.unassignGroupRole role user (group ^. groupID)
            -- THEN
            role' <- App.allGroupRolesIn user (group ^. groupID)
            pure $ do
              -- outcome: the role is unassigned
              role' `shouldBe` []

        it "leaves other roles untouched" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = GroupMember
                otherrole = GroupModerator
                user = ID 1
            group  <- App.addGroup (CreateGroup "title" "desc" [] [])
            ()     <- App.assignGroupRole role user (group ^. groupID)
            ()     <- App.assignGroupRole otherrole user (group ^. groupID)
            roles1 <- App.allGroupRolesIn user (group ^. groupID)
            -- WHEN
            ()     <- App.unassignGroupRole role user (group ^. groupID)
            -- THEN
            roles2 <- App.allGroupRolesIn user (group ^. groupID)
            pure $ do
              -- outcome: the role is unassigned
              sort roles1 `shouldBe` sort [role, otherrole]
              roles2 `shouldBe` [otherrole]

      context "role previously unassigned" $ do
        it "does nothing" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = GroupMember
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            -- WHEN
            ()    <- App.unassignGroupRole role user (group ^. groupID)
            -- THEN
            role' <- App.allGroupRolesIn user (group ^. groupID)
            pure $ do
              -- outcome: the role is unassigned
              role' `shouldBe` []

      it "role is never in new role set, old and new role are equal except for assigned role" $
        \(runner :: RoleAppRunner) -> do
          property $ \(role :: GroupRole, roles :: [GroupRole]) -> do
            join . runner $ do
              let uid = ID 1
              group       <- App.addGroup (CreateGroup "title" "desc" [] [])
              ()          <- forM_ roles $ \r -> App.assignGroupRole r uid (group ^. groupID)
              rolesBefore <- App.allGroupRolesIn uid (group ^. groupID)
              ()          <- App.unassignGroupRole role uid (group ^. groupID)
              rolesAfter  <- App.allGroupRolesIn uid (group ^. groupID)
              pure $ do
                sort rolesBefore `shouldBe` nub (sort roles)
                rolesAfter `shouldNotContain` [role]
                sort (rolesBefore \\ [role]) `shouldBe` sort (rolesAfter \\ [role])
