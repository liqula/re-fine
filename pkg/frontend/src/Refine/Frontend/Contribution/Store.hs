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

import Refine.Frontend.Prelude

import qualified Data.Map.Strict as M

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types


contributionStateUpdate :: GlobalAction -> ContributionState -> ContributionState
contributionStateUpdate a = localAction a . globalAction a
  where
    localAction (ContributionAction action) st = st
      & csCurrentSelectionWithPx   %~ currentRangeUpdate action
      & csCommentKind              %~ commentKindUpdate action
      & csDisplayedContributionID  %~ displayedContributionUpdate action
      & csHighlightedMarkAndBubble %~ highlightedMarkAndBubbleUpdate action
      & csMarkPositions            %~ markPositionsUpdate action
      & csBubblePositioning        %~ bubblePositioningUpdate action
      & csBubbleFilter             %~ bubbleFilterUpdate action
    localAction _ st = st

    globalAction action st = st
      & csQuickCreateShowState     %~ quickCreateShowStateUpdate action
      & csActiveDialog             %~ activeDialogUpdate action


currentRangeUpdate :: ContributionAction -> Maybe SelectionStateWithPx -> Maybe SelectionStateWithPx
currentRangeUpdate action = case action of
  SetRange range -> const (Just range)
  ClearRange     -> const Nothing
  _ -> id

commentKindUpdate :: ContributionAction -> Maybe CommentKind -> Maybe CommentKind
commentKindUpdate action st = case action of
  (SetCommentKind k) -> Just k
  HideCommentEditor  -> Nothing  -- when closing the comment editor, reset the choice
  _ -> st

displayedContributionUpdate :: ContributionAction -> Maybe ContributionID -> Maybe ContributionID
displayedContributionUpdate action st = case action of
  ShowContributionDialog cid'
    | st == Just cid' -> Nothing
    | otherwise       -> Just cid'
  HideContributionDialog -> Nothing
  _ -> st

activeDialogUpdate :: GlobalAction -> Maybe ActiveDialog -> Maybe ActiveDialog
activeDialogUpdate = \case
  ContributionAction ShowCommentEditor   -> const $ Just ActiveDialogComment
  ContributionAction HideCommentEditor   -> const Nothing
  DocumentAction RequestDocumentSave     -> const $ Just ActiveDialogEdit
  DocumentAction (DocumentSave _)        -> const Nothing
  DocumentAction DocumentCancelSave      -> const Nothing
  _ -> id

highlightedMarkAndBubbleUpdate :: ContributionAction -> [ContributionID] -> [ContributionID]
highlightedMarkAndBubbleUpdate (HighlightMarkAndBubble cids) _    = cids
highlightedMarkAndBubbleUpdate _                             cids = cids

quickCreateShowStateUpdate :: GlobalAction -> QuickCreateShowState -> QuickCreateShowState
quickCreateShowStateUpdate action st = case action of
  ContributionAction (SetRange _)               -> somethingWasSelected
  ContributionAction ClearRange                 -> selectionWasRemoved
  HeaderAction ToggleCommentToolbarExtension    -> toolbarWasToggled
  HeaderAction StartTextSpecificComment         -> QuickCreateBlocked
  HeaderAction ToggleEditToolbarExtension       -> toolbarWasToggled
  HeaderAction (StartEdit _)                    -> QuickCreateNotShown  -- (article is hidden, so
                                                                        -- quick create buttons are
                                                                        -- never triggered.)
  HeaderAction CloseToolbarExtension            -> toolbarWasToggled
  _ -> st
  where
    somethingWasSelected = case st of
      QuickCreateShown     -> QuickCreateShown
      QuickCreateNotShown  -> QuickCreateShown
      QuickCreateBlocked   -> QuickCreateBlocked

    selectionWasRemoved = case st of
      QuickCreateShown     -> QuickCreateNotShown
      QuickCreateNotShown  -> QuickCreateNotShown
      QuickCreateBlocked   -> QuickCreateBlocked

    toolbarWasToggled = case st of
      QuickCreateShown     -> QuickCreateNotShown
      QuickCreateNotShown  -> QuickCreateNotShown
      QuickCreateBlocked   -> QuickCreateNotShown

markPositionsUpdate :: ContributionAction -> MarkPositions -> MarkPositions
markPositionsUpdate (SetMarkPositions positions) = markPositionsMap .~ M.fromList positions
markPositionsUpdate _ = id

bubblePositioningUpdate :: ContributionAction -> BubblePositioning -> BubblePositioning
bubblePositioningUpdate (SetBubblePositioning strategy) _ = strategy
bubblePositioningUpdate _ st = st

bubbleFilterUpdate :: ContributionAction -> Maybe (Set ContributionID) -> Maybe (Set ContributionID)
bubbleFilterUpdate (SetBubbleFilter f) _ = f
bubbleFilterUpdate _ st = st
