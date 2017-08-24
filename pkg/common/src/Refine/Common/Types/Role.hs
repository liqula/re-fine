{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.Types.Role where

import Refine.Common.Prelude


data GroupRole
  = GroupMember
  | GroupModerator
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

data GlobalRole
  = GlobalAdmin
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

makeRefineTypes [''GroupRole, ''GlobalRole]
