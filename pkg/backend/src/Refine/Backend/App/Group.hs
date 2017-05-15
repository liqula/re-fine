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

module Refine.Backend.App.Group where

import Control.Lens ((^.))

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Common.ChangeAPI
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude


-- * group manipulation

addGroup :: Create Group -> App Group
addGroup group = do
  appLog "addGroup"
  db $ DB.createGroup group

getGroup :: ID Group -> App Group
getGroup group = do
  appLog "readGroup"
  db $ DB.getGroup group

-- | Modify the group using the new values from the `Create Group` information.
modifyGroup :: ID Group -> Create Group -> App Group
modifyGroup groupId group = do
  appLog "modifyGroup"
  db $ DB.modifyGroup groupId group

-- | Remove a group and cleans up dangling references.
--
-- The users won't be transitive members of supergroups any more.
--
removeGroup :: ID Group -> App ()
removeGroup groupId = do
  appLog "removeGroup"
  db $ DB.removeGroup groupId

-- * subgroups

changeSubGroup :: ChangeSubGroup -> App ()
changeSubGroup csg = do
  appLog "changeSubGroup"
  let cmd = case csg of
              AddSubGroup{} -> Refine.Backend.App.Group.addSubGroup
              RmSubGroup{}  -> Refine.Backend.App.Group.removeSubGroup
  cmd (csg ^. csgParent) (csg ^. csgChild)

-- | Add a new child group to a group
addSubGroup :: ID Group -> ID Group -> App ()
addSubGroup parent child = do
  appLog "addSubGroup"
  db $ DB.addSubGroup parent child

-- | Remove a child group from a parent
removeSubGroup :: ID Group -> ID Group -> App ()
removeSubGroup parent child = do
  appLog "removeSubGroup"
  db $ DB.removeSubGroup parent child
