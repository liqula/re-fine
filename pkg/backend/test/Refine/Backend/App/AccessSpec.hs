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
module Refine.Backend.App.AccessSpec where

import Control.Lens
import Control.Monad (join)
import Test.Hspec

import Refine.Backend.App.Core   as App
import Refine.Backend.App.Access as App
import Refine.Backend.App.Group  as App
import Refine.Backend.Database
import Refine.Backend.User
import Refine.Common.Types.Access
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude (ID(..))
import Refine.Test.App.Runner

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


type AppRunner a = AppM DB FreeUH a -> IO a

spec :: Spec
spec = do
  describe "Refine.Backend.App.Access" . around provideDevModeAppRunner $ do
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
          role' <- App.getRole user (group ^. groupID)
          pure $ do
            -- outcome: new role is assigned
            role' `shouldBe` Just role

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
          role' <- App.getRole user (group ^. groupID)
          pure $ do
            -- outcome: the role stays
            role' `shouldBe` Just role

      it "overwrite new role" $ \(runner :: AppRunner (IO ())) -> do
        -- There is only one role for the user.
        join . runner $ do
          -- GIVEN
          let oldrole = Member
              newrole = Moderator
              user = ID 1
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          -- WHEN
          () <- App.assignRole oldrole user (group ^. groupID)
          () <- App.assignRole newrole user (group ^. groupID)
          -- THEN
          role' <- App.getRole user (group ^. groupID)
          pure $ do
            -- ouctome: the new role is there
            role' `shouldBe` Just newrole

    describe "Unassign role" $ do
      it "unassign the given role" $ \(runner :: AppRunner (IO ())) -> do
        -- There is only one role for the user.
        join . runner $ do
          -- GIVEN
          let role = Member
              user = ID 1
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          ()    <- App.assignRole role user (group ^. groupID)
          -- WHEN
          ()    <- App.unassignRole role user (group ^. groupID)
          -- THEN
          role' <- App.getRole user (group ^. groupID)
          pure $ do
            -- outcome: the role is unassigned
            role' `shouldBe` Nothing

      it "unassign different role" $ \(runner :: AppRunner (IO ())) -> do
        -- There is only one role for the user.
        join . runner $ do
          -- GIVEN
          let role = Member
              otherrole = Moderator
              user = ID 1
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          ()    <- App.assignRole role user (group ^. groupID)
          -- WHEN
          ()    <- App.unassignRole otherrole user (group ^. groupID)
          -- THEN
          role' <- App.getRole user (group ^. groupID)
          pure $ do
            -- outcome: the role is unassigned
            role' `shouldBe` Just role

      it "unassign role when role is not assigned" $ \(runner :: AppRunner (IO ())) -> do
        -- There is only one role for the user.
        join . runner $ do
          -- GIVEN
          let role = Member
              user = ID 1
          group <- App.addGroup (CreateGroup "title" "desc" [] [])
          -- WHEN
          ()    <- App.unassignRole role user (group ^. groupID)
          -- THEN
          role' <- App.getRole user (group ^. groupID)
          pure $ do
            -- outcome: the role is unassigned
            role' `shouldBe` Nothing
