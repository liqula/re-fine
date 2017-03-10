{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Refine.Common.Types.Group where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude.TH (makeRefineType)


data CreateGroup = CreateGroup {
    _createGroupTitle :: ST
  , _createGroupDesc  :: ST
  , _createGroupParents  :: [ID Group]
  , _createGroupChildren :: [ID Group]
  }
  deriving (Eq, Generic)

data Group = Group {
    _groupID    :: ID Group
  , _groupTitle :: ST
  , _groupDesc  :: ST
  , _groupParents  :: [ID Group]
  , _groupChildren :: [ID Group]
  }
  deriving (Eq, Generic)

-- * create types

type instance Create Group = CreateGroup

-- * refine types

makeRefineType ''CreateGroup
makeRefineType ''Group
