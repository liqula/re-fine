{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.Types.Contribution where

import Refine.Common.Prelude

import Data.String.Conversions (ST)

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
