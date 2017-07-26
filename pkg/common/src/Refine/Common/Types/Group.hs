{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Refine.Common.Types.Group where

import Refine.Common.Prelude

import Control.Lens (Lens')
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude


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
  { _groupMetaID    :: MetaID Group
  , _groupTitle     :: ST
  , _groupDesc      :: ST
  , _groupParents   :: [ID Group]
  , _groupChildren  :: [ID Group]
  }
  deriving (Eq, Generic, Show)

type instance Create Group = CreateGroup

-- | There is a special way to refer to a group called 'UniversalGroup', which is the root that
-- always exists.
universalGroup :: ID Group
universalGroup = ID 0

makeRefineTypes [''CreateGroup, ''Group]

groupID :: Lens' Group (ID Group)
groupID = groupMetaID . miID
