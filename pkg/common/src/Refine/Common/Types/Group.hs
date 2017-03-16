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
  }
  deriving (Eq, Generic, Show)

-- | A *process* has a certain type (more general: "collecting wild ideas",
-- "collaborative text editing", ...; or more specific: "spending a school budget", "updating a
-- party constitution", "setting up a Ltd.", ...) that determines much of what it looks like and how
-- it works.
--
-- Like groups, processes are initiated by users.  Unlike groups, a process always has exactly one
-- parent group ("its *home group*", or just "its group") and no children.
--
-- > data Process = Process (UID Process) MetaInfo Title Description (UID Group)
--
-- FIXME: not implemented yet.
data Process

type instance Create Group = CreateGroup

makeRefineType ''CreateGroup
makeRefineType ''Group
