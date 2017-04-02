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
import qualified Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Types


contributionStateUpdate :: ContributionAction -> ContributionState -> ContributionState
contributionStateUpdate action state = state
  & csCurrentSelection         %~ currentSelectionUpdate action
  & csCommentKind              %~ commentKindUpdate action
  & csDisplayedContributionID  %~ displayedContributionUpdate action
  & csCommentEditorIsVisible   %~ commentEditorIsVisibleUpdate action
  & csHighlightedMarkAndBubble %~ highlightedMarkAndBubbleUpdate action
  & csMarkPositions            %~ markPositionsUpdate action


currentSelectionUpdate :: ContributionAction -> Selection -> Selection
currentSelectionUpdate action state = case action of
  (UpdateSelection newState _) -> newState
  ShowCommentEditor _          -> NothingSelected
  SubmitEdit                   -> NothingSelected
  HideCommentEditor            -> NothingSelected
  _ -> state

commentKindUpdate :: ContributionAction -> Maybe CommentKind -> Maybe CommentKind
commentKindUpdate action state = case action of
  (SetCommentKind k) -> Just k
  HideCommentEditor  -> Nothing -- when closing the comment editor, reset the selection
  _ -> state

displayedContributionUpdate :: ContributionAction -> Maybe ContributionID -> Maybe ContributionID
displayedContributionUpdate action state = case action of
  ShowContributionDialog contributionId -> Just contributionId
  HideCommentOverlay                    -> Nothing
  _ -> state

commentEditorIsVisibleUpdate :: ContributionAction -> ContributionEditorData -> ContributionEditorData
commentEditorIsVisibleUpdate action state = case action of
  ShowCommentEditor curSelection                                                  -> EditorIsVisible curSelection
  UpdateSelection (RangeSelected range _) HT.CommentToolbarExtensionWithSelection -> EditorIsVisible (Just range)
  HideCommentEditor                                                               -> EditorIsHidden
  _ -> state

highlightedMarkAndBubbleUpdate :: ContributionAction -> Maybe ContributionID -> Maybe ContributionID
highlightedMarkAndBubbleUpdate action state = case action of
  (HighlightMarkAndBubble dataChunkId) -> Just dataChunkId
  UnhighlightMarkAndBubble             -> Nothing
  _ -> state

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
