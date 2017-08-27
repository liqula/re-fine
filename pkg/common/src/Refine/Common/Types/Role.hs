{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Types.Role where
#include "import_common.hs"


data GroupRole
  = GroupMember
  | GroupModerator
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

data GlobalRole
  = GlobalAdmin
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

makeRefineTypes [''GroupRole, ''GlobalRole]
