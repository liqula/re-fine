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

import           Control.Lens ((&), (%~), (^.))
import qualified Data.Map.Strict as M

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Header.Types as HT


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
  (AddMarkPosition dataChunkId newMarkPosition)
    -> let upd       = Just . maybe newMarkPosition (updTop . updBottom)
           updTop    = markPositionTop    %~ min (newMarkPosition ^. markPositionTop)
           updBottom = markPositionBottom %~ max (newMarkPosition ^. markPositionBottom)
       in MarkPositions $ M.alter upd dataChunkId (_unMarkPositions state)
  _ -> state
