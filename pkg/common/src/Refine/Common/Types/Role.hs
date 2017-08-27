{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.Types.Role where
#include "import.hs"


data GroupRole
  = GroupMember
  | GroupModerator
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

data GlobalRole
  = GlobalAdmin
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

makeRefineTypes [''GroupRole, ''GlobalRole]
