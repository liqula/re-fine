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
import Control.Monad (join)
import Data.List (sort)
import Test.Hspec

import Refine.Backend.App.Core   as App
import Refine.Backend.App.Group  as App
import Refine.Backend.App.Role   as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Backend.User
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude (ID(..))
import Refine.Common.Types.Role

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


type AppRunner a = AppM DB FreeUH a -> IO a

spec :: Spec
spec = around provideDevModeAppRunner $ do
    describe "Assign role" $ do
      it "assign new role" $ \(runner :: AppRunner (IO ())) -> do
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

      it "assign the same role" $ \(runner :: AppRunner (IO ())) -> do
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

      it "assign a role which does not overrule the actual one" $ \(runner :: AppRunner (IO ())) -> do
        join . runner $ do
          -- GIVEN
          let role = Member
              user = ID 1
              newRole = ReadOnly
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          -- WHEN
          () <- App.assignRole role user (group ^. groupID)
          () <- App.assignRole newRole user (group ^. groupID)
          -- THEN
          roles <- App.allRoles user (group ^. groupID)
          pure $ do
            -- outcome: the assigned role is not present for the user
            roles `shouldBe` [role]

      it "assing a role which does overrule the actual one" $ \(runner :: AppRunner (IO ())) -> do
        join . runner $ do
          -- GIVEN
          let role = ReadOnly
              user = ID 1
              newRole = Member
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          -- WHEN
          () <- App.assignRole role user (group ^. groupID)
          () <- App.assignRole newRole user (group ^. groupID)
          -- THEN
          roles <- App.allRoles user (group ^. groupID)
          pure $ do
            -- outcome: the new rule removes the old one, and the new stays
            roles `shouldBe` [newRole]

      it "assing a role which is not associated with the actual one" $ \(runner :: AppRunner (IO ())) -> do
        -- outcome: the new rules is added to the list
        join . runner $ do
          -- GIVEN
          let role = ReadOnly
              user = ID 1
              otherRole = GroupInitiator
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          -- WHEN
          () <- App.assignRole role user (group ^. groupID)
          () <- App.assignRole otherRole user (group ^. groupID)
          -- THEN
          roles <- App.allRoles user (group ^. groupID)
          pure $ do
            -- outcome: the new rule removes the old one, and the new stays
            sort roles `shouldBe` sort [role, otherRole]

    describe "Unassign role" $ do
      it "unassign the given role" $ \(runner :: AppRunner (IO ())) -> do
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

      it "unassign a role which is overruled, and there is a role which is the diff" $ \_runner -> do
        -- outcome: a role which express exactly the difference, that role will be the new one
        pending

      it "unassign a role which is overruled, and there is no other role which is the diff" $ \_runner -> do
        -- outcome: the overrule role will be removed
        pending

      it "unassign a role which overrules the actual one" $ \_runner -> do
        -- outcome: the role will be removed
        pending

      it "unassign a role from the roleset" $ \(runner :: AppRunner (IO ())) -> do
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
          ()     <- App.unassignRole otherrole user (group ^. groupID)
          -- THEN
          roles2 <- App.allRoles user (group ^. groupID)
          pure $ do
            -- outcome: the role is unassigned
            sort roles1 `shouldBe` sort [role, otherrole]
            roles2 `shouldBe` [role]

      it "unassign role when role is not assigned" $ \(runner :: AppRunner (IO ())) -> do
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
