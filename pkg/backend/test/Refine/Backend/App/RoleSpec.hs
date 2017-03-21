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

import Control.Lens
import Control.Monad (join, forM_)
import Data.List (sort, nub, (\\))
import Test.Hspec
import Test.QuickCheck

import Refine.Backend.App.Core   as App
import Refine.Backend.App.Group  as App
import Refine.Backend.App.Role   as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Backend.User
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude (ID(..))
import Refine.Common.Types.Role

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


type RoleAppRunner = AppM DB FreeUH (IO ()) -> IO (IO ())

spec :: Spec
spec = around provideDevModeAppRunner $ do
    describe "assign role" $ do
      context "role was not previously assigned" $ do
        it "adds role" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = Member
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            -- WHEN
            ()    <- App.assignRole role user (group ^. groupID)
            -- THEN
            roles <- App.allRoles user (group ^. groupID)
            pure $ do
              -- outcome: new role is assigned
              roles `shouldBe` [role]

      context "role was previously assigned" $ do
        it "does nothing" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = Member
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            -- WHEN
            () <- App.assignRole role user (group ^. groupID)
            () <- App.assignRole role user (group ^. groupID)
            -- THEN
            roles <- App.allRoles user (group ^. groupID)
            pure $ do
              -- outcome: the role stays
              roles `shouldBe` [role]

      it "role is always in new role set, old and new role are equal except for assigned role" $
        \(runner :: RoleAppRunner) -> do
          property $ \(role :: Role, roles :: [Role]) -> do
            join . runner $ do
              let uid = ID 1
              group       <- App.addGroup (CreateGroup "title" "desc" [] [])
              ()          <- forM_ roles $ \r -> App.assignRole r uid (group ^. groupID)
              rolesBefore <- App.allRoles uid (group ^. groupID)
              ()          <- App.assignRole role uid (group ^. groupID)
              rolesAfter  <- App.allRoles uid (group ^. groupID)
              pure $ do
                sort rolesBefore `shouldBe` (nub $ sort roles)
                rolesAfter `shouldContain` [role]
                sort (rolesBefore \\ [role]) `shouldBe` sort (rolesAfter \\ [role])

    describe "### unassign role" $ do
      context "role previously assigned" $ do
        it "unassigns the given role" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = Member
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            ()    <- App.assignRole role user (group ^. groupID)
            -- WHEN
            ()    <- App.unassignRole role user (group ^. groupID)
            -- THEN
            role' <- App.allRoles user (group ^. groupID)
            pure $ do
              -- outcome: the role is unassigned
              role' `shouldBe` []

        it "leaves other roles untouched" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = Member
                otherrole = GroupInitiator
                user = ID 1
            group  <- App.addGroup (CreateGroup "title" "desc" [] [])
            ()     <- App.assignRole role user (group ^. groupID)
            ()     <- App.assignRole otherrole user (group ^. groupID)
            roles1 <- App.allRoles user (group ^. groupID)
            -- WHEN
            ()     <- App.unassignRole role user (group ^. groupID)
            -- THEN
            roles2 <- App.allRoles user (group ^. groupID)
            pure $ do
              -- outcome: the role is unassigned
              sort roles1 `shouldBe` sort [role, otherrole]
              roles2 `shouldBe` [otherrole]

      context "role previously unassigned" $ do
        it "does nothing" $ \(runner :: RoleAppRunner) -> do
          join . runner $ do
            -- GIVEN
            let role = Member
                user = ID 1
            group <- App.addGroup (CreateGroup "title" "desc" [] [])
            -- WHEN
            ()    <- App.unassignRole role user (group ^. groupID)
            -- THEN
            role' <- App.allRoles user (group ^. groupID)
            pure $ do
              -- outcome: the role is unassigned
              role' `shouldBe` []

      it "role is never in new role set, old and new role are equal except for assigned role" $
        \(runner :: RoleAppRunner) -> do
          property $ \(role :: Role, roles :: [Role]) -> do
            join . runner $ do
              let uid = ID 1
              group       <- App.addGroup (CreateGroup "title" "desc" [] [])
              ()          <- forM_ roles $ \r -> App.assignRole r uid (group ^. groupID)
              rolesBefore <- App.allRoles uid (group ^. groupID)
              ()          <- App.unassignRole role uid (group ^. groupID)
              rolesAfter  <- App.allRoles uid (group ^. groupID)
              pure $ do
                sort rolesBefore `shouldBe` (nub $ sort roles)
                rolesAfter `shouldNotContain` [role]
                sort (rolesBefore \\ [role]) `shouldBe` sort (rolesAfter \\ [role])
