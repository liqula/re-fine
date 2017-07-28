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

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Role where

import Refine.Backend.Prelude

import Control.Lens ((^.))
import Control.Monad (when, unless)

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Common.Types
import Refine.Common.ChangeAPI


changeRole :: ChangeRole -> App ()
changeRole cr = do
  appLog "changeRole"
  let cmd = case cr of
              AssignRole {}   -> Refine.Backend.App.Role.assignRole
              UnassignRole {} -> Refine.Backend.App.Role.unassignRole
  cmd (cr ^. crRole) (cr ^. crUser) (cr ^. crGroupRef)

-- | Assign a role lenitently to a user in a group. If the role is already overruled
-- the new role is not assigned. If the role is overrules the overruled one
-- is removed.
assignRole :: Role -> ID User -> ID Group -> App ()
assignRole role uid gid = do
  appLog "assignRole"
  db $ do
    roles <- DB.getRoles gid uid
    unless (role `elem` roles) $ DB.assignRole gid uid role

-- | Unassign a role from a user in a group.
unassignRole :: Role -> ID User -> ID Group -> App ()
unassignRole role uid gid = do
  appLog "unassignRole"
  db $ do
    actRoles <- DB.getRoles gid uid
    when (role `elem` actRoles) $ DB.unassignRole gid uid role

-- | Return the roles of a user
allRoles :: ID User -> ID Group -> App [Role]
allRoles uid gid = do
  appLog "allRoles"
  db $ DB.getRoles gid uid
