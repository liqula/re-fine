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

module Refine.Backend.App.Access where

import Control.Lens ((^.))
import Control.Monad (when)

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Common.Types
import Refine.Common.ChangeAPI


changeRole :: ChangeRole -> App ()
changeRole cr = do
  appLog "changeRole"
  let cmd = case cr of
              AssignRole{}   -> Refine.Backend.App.Access.assignRole
              UnassignRole{} -> Refine.Backend.App.Access.unassignRole
  cmd (cr ^. crRole) (cr ^. crUser) (cr ^. crGroup)

-- | A user is assigned to a group (and not to subgroups).
assignRole :: Role -> ID User -> ID Group -> App ()
assignRole role uid gid = do
  appLog "assignRole"
  db $ DB.assignRole gid uid role

-- | Unassign a role from a user in a group.
unassignRole :: Role -> ID User -> ID Group -> App ()
unassignRole role uid gid = do
  appLog "unassignRole"
  db $ do
    actRole <- DB.getRole gid uid
    when (Just role == actRole) $ DB.unassignRole gid uid role

-- | Return (Just role) if the user has a role in that group
-- otherwise Nothing.
getRole :: ID User -> ID Group -> App (Maybe Role)
getRole uid gid = do
  appLog "getRole"
  db $ DB.getRole gid uid
