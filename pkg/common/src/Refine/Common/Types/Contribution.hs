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

import Refine.Common.Types.Prelude
import Refine.Common.Types.Core


class IsContribution a where
  contribID :: ID a -> ContributionID

instance IsContribution Note where
  contribID = ContribIDNote

instance IsContribution Discussion where
  contribID = ContribIDDiscussion

instance IsContribution Edit where
  contribID = ContribIDEdit

contributionIDToKindST :: ContributionID -> ST
contributionIDToKindST (ContribIDNote _)       = "note"
contributionIDToKindST (ContribIDDiscussion _) = "discussion"
contributionIDToKindST (ContribIDEdit _)       = "edit"


-- This can probably solved with lenses, but we don't know how...
getNoteID :: ContributionID -> Maybe (ID Note)
getNoteID (ContribIDNote i)       = Just i
getNoteID (ContribIDDiscussion _) = Nothing
getNoteID (ContribIDEdit _)       = Nothing

-- This can probably solved with lenses, but we don't know how...
getDiscussionID :: ContributionID -> Maybe (ID Discussion)
getDiscussionID (ContribIDNote _)       = Nothing
getDiscussionID (ContribIDDiscussion i) = Just i
getDiscussionID (ContribIDEdit _)       = Nothing
