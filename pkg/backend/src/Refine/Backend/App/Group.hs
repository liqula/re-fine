{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Group where

import Refine.Backend.Prelude

import qualified Data.Set as Set

import           Refine.Backend.App.Access
import           Refine.Backend.App.Core
import           Refine.Backend.Database.Class as DB
import qualified Refine.Common.Access.Policy as AP
import           Refine.Common.ChangeAPI
import           Refine.Common.Types


-- * group manipulation

addGroup :: CreateGroup -> App Group
addGroup cgroup = do
  appLog "addGroup"
  assertCreds AP.addGroup
  group <- db $ DB.createGroup cgroup
  invalidateCaches $ Set.fromList [CacheKeyGroupIds]
  pure group

getGroup :: ID Group -> App Group
getGroup gid = do
  appLog "getGroup"
  assertCreds $ AP.getGroup gid
  db $ DB.getGroup gid

getGroups :: App [Group]
getGroups = do
  appLog "getGroups"
  let allow = AP.getGroup . view groupID
  filterByCreds allow =<< db DB.getGroups

-- | Modify the group using the new values from the `Create Group` information.
modifyGroup :: ID Group -> CreateGroup -> App Group
modifyGroup gid group' = do
  appLog "modifyGroup"
  assertCreds $ AP.updateGroup gid
  gr <- db $ DB.modifyGroup gid group'
  invalidateCaches $ Set.fromList [CacheKeyGroup gid]
  pure gr

-- | Remove a group and cleans up dangling references.
--
-- The users won't be transitive members of supergroups any more.
removeGroup :: ID Group -> App ()
removeGroup gid = do
  appLog "removeGroup"
  assertCreds $ AP.deleteGroup gid
  db $ DB.removeGroup gid


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
