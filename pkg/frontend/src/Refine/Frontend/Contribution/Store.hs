{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Contribution.Store where

import           Control.Lens ((&), (%~), (.~), (^.), to)
import           Data.List (foldl')
import qualified Data.Map.Strict as M

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types


contributionStateUpdate :: GlobalAction -> ContributionState -> ContributionState
contributionStateUpdate a = localAction a . globalAction a
  where
    localAction (ContributionAction action) state = state
      & csCurrentRange             %~ currentRangeUpdate action
      & csCommentKind              %~ commentKindUpdate action
      & csDisplayedContributionID  %~ displayedContributionUpdate action
      & csCommentEditorVisible     %~ commentEditorVisibleUpdate action
      & csHighlightedMarkAndBubble %~ highlightedMarkAndBubbleUpdate action
      & csMarkPositions            %~ markPositionsUpdate action
    localAction _ state = state

    globalAction action state = state
      & csQuickCreateShowState     %~ quickCreateShowStateUpdate action


currentRangeUpdate :: ContributionAction -> Maybe Range -> Maybe Range
currentRangeUpdate action = case action of
  (UpdateRange mrange) -> const mrange
  _ -> id

commentKindUpdate :: ContributionAction -> Maybe CommentKind -> Maybe CommentKind
commentKindUpdate action state = case action of
  (SetCommentKind k) -> Just k
  HideCommentEditor  -> Nothing  -- when closing the comment editor, reset the choice
  _ -> state

displayedContributionUpdate :: ContributionAction -> Maybe ContributionID -> Maybe ContributionID
displayedContributionUpdate action state = case action of
  ShowContributionDialog contributionId -> Just contributionId
  HideCommentOverlay                    -> Nothing
  _ -> state

commentEditorVisibleUpdate :: ContributionAction -> Bool -> Bool
commentEditorVisibleUpdate = \case
  ShowCommentEditor -> const True
  HideCommentEditor -> const False
  _ -> id

highlightedMarkAndBubbleUpdate :: ContributionAction -> Maybe ContributionID -> Maybe ContributionID
highlightedMarkAndBubbleUpdate action state = case action of
  (HighlightMarkAndBubble dataChunkId) -> Just dataChunkId
  UnhighlightMarkAndBubble             -> Nothing
  _ -> state

quickCreateShowStateUpdate :: GlobalAction -> QuickCreateShowState -> QuickCreateShowState
quickCreateShowStateUpdate action state = case action of
  ContributionAction (UpdateRange (Just _))     -> somethingWasSelected
  ContributionAction (UpdateRange Nothing)      -> selectionWasRemoved
  HeaderAction ToggleCommentToolbarExtension    -> toolbarWasToggled
  HeaderAction StartTextSpecificComment         -> QuickCreateBlocked
  HeaderAction ToggleEditToolbarExtension       -> toolbarWasToggled
  HeaderAction (StartEdit _)                    -> QuickCreateBlocked
  HeaderAction CloseToolbarExtension            -> toolbarWasToggled
  _ -> state
  where
    somethingWasSelected = case state of
      QuickCreateShown     -> QuickCreateShown
      QuickCreateNotShown  -> QuickCreateShown
      QuickCreateBlocked   -> QuickCreateBlocked

    selectionWasRemoved = case state of
      QuickCreateShown     -> QuickCreateNotShown
      QuickCreateNotShown  -> QuickCreateNotShown
      QuickCreateBlocked   -> QuickCreateBlocked

    toolbarWasToggled = case state of
      QuickCreateShown     -> QuickCreateNotShown
      QuickCreateNotShown  -> QuickCreateNotShown
      QuickCreateBlocked   -> QuickCreateNotShown

markPositionsUpdate :: ContributionAction -> MarkPositions -> MarkPositions
markPositionsUpdate action state = case action of
  (ScheduleAddMarkPosition dataChunkId newMarkPosition)
    -> let upd       = Just . maybe newMarkPosition (updTop . updBottom)
           updTop    = markPositionTop    %~ min (newMarkPosition ^. markPositionTop)
           updBottom = markPositionBottom %~ max (newMarkPosition ^. markPositionBottom)

          -- FIXME: i'm not too confident this is going into the right direction.  why are marks
          -- rendered so many times before they finally have a stable bounding rectangle?

       in state & markPositionsScheduled %~ M.alter upd dataChunkId
  DischargeAddMarkPositions
    -> let upd :: M.Map ContributionID MarkPosition -> M.Map ContributionID MarkPosition
           upd firstm = foldl' (\m (i, p) -> M.insert i p m) firstm (state ^. markPositionsScheduled. to M.toList)
       in state & markPositionsMap       %~ upd
                & markPositionsScheduled .~ mempty
  _ -> state
