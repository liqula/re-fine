{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Role where
#include "import_backend.hs"

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Common.Types
import Refine.Common.ChangeAPI


-- | Assign or unassign 'GroupRole' or 'GlobalRole'.
changeRole :: ChangeRole -> App ()
changeRole cr = do
  appLog "changeRole"
  case cr of
    AssignGroupRole  u r g  -> Refine.Backend.App.Role.assignGroupRole r u g
    UnassignGroupRole u r g -> Refine.Backend.App.Role.unassignGroupRole r u g
    AssignGlobalRole  u r   -> Refine.Backend.App.Role.assignGlobalRole r u
    UnassignGlobalRole u r  -> Refine.Backend.App.Role.unassignGlobalRole r u


-- * group roles

-- | Assign a role lenitently to a user in a group. If the role is already overruled
-- the new role is not assigned. If the role is overrules the overruled one
-- is removed.
assignGroupRole :: GroupRole -> ID User -> ID Group -> App ()
assignGroupRole role uid gid = do
  appLog "assignGroupRole"
  db $ do
    roles <- DB.getGroupRolesIn gid uid
    unless (role `elem` roles) $ DB.assignGroupRole gid uid role

-- | Unassign a role from a user in a group.
unassignGroupRole :: GroupRole -> ID User -> ID Group -> App ()
unassignGroupRole role uid gid = do
  appLog "unassignGroupRole"
  db $ do
    actRoles <- DB.getGroupRolesIn gid uid
    when (role `elem` actRoles) $ DB.unassignGroupRole gid uid role

-- | Return the roles of a user
allGroupRolesIn :: ID User -> ID Group -> App [GroupRole]
allGroupRolesIn uid gid = do
  appLog "allGroupRolesIn"
  db $ DB.getGroupRolesIn gid uid

-- | Return the roles of a user
allGroupRoles :: ID User -> App [(GroupRole, ID Group)]
allGroupRoles uid = do
  appLog "allGroupRoles"
  db $ DB.getGroupRoles uid


-- * global roles

assignGlobalRole :: GlobalRole -> ID User -> App ()
assignGlobalRole role uid = do
  appLog "assignGlobalRole"
  db $ do
    roles <- DB.getGlobalRoles uid
    unless (role `elem` roles) $ DB.assignGlobalRole uid role

unassignGlobalRole :: GlobalRole -> ID User -> App ()
unassignGlobalRole role uid = do
  appLog "unassignGlobalRole"
  db $ do
    actRoles <- DB.getGlobalRoles uid
    when (role `elem` actRoles) $ DB.unassignGlobalRole uid role

allGlobalRoles :: ID User -> App [GlobalRole]
allGlobalRoles uid = do
  appLog "allGlobalRoles"
  db $ DB.getGlobalRoles uid
