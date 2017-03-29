{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Refine.Common.Types.Group where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude.TH (makeRefineType)


data CreateGroup = CreateGroup
  { _createGroupTitle :: ST
  , _createGroupDesc  :: ST
  , _createGroupParents  :: [ID Group]
  , _createGroupChildren :: [ID Group]
  , _createGroupUniversal :: Bool
  }
  deriving (Eq, Generic, Show)

-- | *Groups* are the entities that represent organisations ("zerobuzz.net"),
-- sub-organisations ("zerobuzz developers", "zerobuzz board"), or special interests ("nuke the
-- whales", "equal rights for part-time employees").
--
-- Users can CRUD (Create/Read/Update/Delete) groups.  A user who creates a group has the
-- 'GroupInitiator' 'Role' in that group.
--
-- Groups form a directed acyclic graph (DAG): Every group can have one or more children, but other
-- than in a tree-shaped hierarchy, every child can have multiple parents.
data Group = Group
  { _groupID    :: ID Group
  , _groupTitle :: ST
  , _groupDesc  :: ST
  , _groupParents  :: [ID Group]
  , _groupChildren :: [ID Group]
  , _groupUniversal :: Bool
  }
  deriving (Eq, Generic, Show)

type instance Create Group = CreateGroup

-- | There is a special way to refer to a group called 'UniversalGroup', which is the root that
-- always exists.  'GroupRef' is a way to refer to either the universal group or some group that we
-- have the 'ID' of.
--
-- (Ultimately, we may not want to have this type around here, becasue there won't be a universal
-- group in which people do stuff in the portal once it has grown more mature.  For now it's a
-- useful and adequate abstraction.)
data GroupRef = GroupRef (ID Group) | UniversalGroup
  deriving (Eq, Show, Generic)

makeRefineType ''CreateGroup
makeRefineType ''Group
makeRefineType ''GroupRef
