{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Contribution where

import Refine.Common.Prelude

import Data.String.Conversions (ST)

import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Common.Types.Core


class IsContribution a where
  contribID :: ID a -> ContributionID

instance IsContribution Note where
  contribID = ContribIDNote

instance IsContribution Question where
  contribID = ContribIDQuestion

instance IsContribution Discussion where
  contribID = ContribIDDiscussion

instance IsContribution Edit where
  contribID = ContribIDEdit

instance IsContribution HighlightMark where
  contribID _ = ContribIDHighlightMark

contributionIDToKindST :: ContributionID -> ST
contributionIDToKindST (ContribIDNote _)       = "note"
contributionIDToKindST (ContribIDQuestion _)   = "question"
contributionIDToKindST (ContribIDDiscussion _) = "discussion"
contributionIDToKindST (ContribIDEdit _)       = "edit"
contributionIDToKindST ContribIDHighlightMark  = "highlight"


-- This can probably solved with lenses, but we don't know how...
getNoteID :: ContributionID -> Maybe (ID Note)
getNoteID (ContribIDNote i)       = Just i
getNoteID (ContribIDQuestion _)   = Nothing
getNoteID (ContribIDDiscussion _) = Nothing
getNoteID (ContribIDEdit _)       = Nothing
getNoteID  ContribIDHighlightMark = Nothing

-- This can probably solved with lenses, but we don't know how...
getDiscussionID :: ContributionID -> Maybe (ID Discussion)
getDiscussionID (ContribIDNote _)       = Nothing
getDiscussionID (ContribIDQuestion _)   = Nothing
getDiscussionID (ContribIDDiscussion i) = Just i
getDiscussionID (ContribIDEdit _)       = Nothing
getDiscussionID  ContribIDHighlightMark = Nothing
