{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Types.Contribution where
#include "import_common.hs"

import Refine.Common.Types.Prelude
import Refine.Common.Types.Core

{-
class IsContribution a where
  contribID :: ID a -> ContributionID

instance IsContribution Discussion where
  contribID = ContribIDDiscussion

instance IsContribution Edit where
  contribID = ContribIDEdit
-}
contributionIDToKindST :: ContributionID -> ST
contributionIDToKindST (ContribIDDiscussion True _)  = "note"
contributionIDToKindST (ContribIDDiscussion False _) = "discussion"
contributionIDToKindST (ContribIDEdit _)             = "edit"


-- This can probably solved with lenses, but we don't know how...
getDiscussionID :: ContributionID -> Maybe (ID Discussion)
getDiscussionID (ContribIDDiscussion _ i) = Just i
getDiscussionID (ContribIDEdit _)         = Nothing
