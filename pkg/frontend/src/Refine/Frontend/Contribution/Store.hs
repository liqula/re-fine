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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Contribution.Store where

import           Control.Lens ((&), (%~), (^.))
import qualified Data.Map.Strict as M
import           Data.Void

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Header.Types as HT


contributionStateUpdate :: ContributionAction -> ContributionState -> ContributionState
contributionStateUpdate action state =
  let newState = state
                  & csCurrentSelection         %~ currentSelectionUpdate action
                  & csCommentCategory          %~ commentCategoryUpdate action
                  & csDiscussionId             %~ discussionIsVisibleUpdate action
                  & csNoteId                   %~ noteIsVisibleUpdate action
                  & csCommentEditorIsVisible   %~ commentEditorIsVisibleUpdate action
                  & csHighlightedMarkAndBubble %~ highlightedMarkAndBubbleUpdate action
                  & csMarkPositions            %~ markPositionsUpdate action
  in newState


---------------------------------------------------------------------------

currentSelectionUpdate :: ContributionAction -> Selection -> Selection
currentSelectionUpdate action state = case action of
  (UpdateSelection newState _) -> newState
  ShowCommentEditor _          -> NothingSelected
  SubmitEdit                   -> NothingSelected
  HideCommentEditor            -> NothingSelected
  _ -> state

commentCategoryUpdate :: ContributionAction -> Maybe CommentCategory -> Maybe CommentCategory
commentCategoryUpdate action state = case action of
  (SetCommentCategory category) -> Just category
  HideCommentEditor             -> Nothing -- when closing the comment editor, reset the selection
  _ -> state

discussionIsVisibleUpdate :: ContributionAction -> Maybe (ID Discussion) -> Maybe (ID Discussion)
discussionIsVisibleUpdate action state = case action of
  (ShowDiscussionOverlay discussionId) -> Just discussionId
  HideCommentOverlay                   -> Nothing
  _ -> state

noteIsVisibleUpdate :: ContributionAction -> Maybe (ID Note) -> Maybe (ID Note)
noteIsVisibleUpdate action state = case action of
  (ShowNoteOverlay noteId) -> Just noteId
  HideCommentOverlay       -> Nothing
  _ -> state

commentEditorIsVisibleUpdate :: ContributionAction -> ContributionEditorData -> ContributionEditorData
commentEditorIsVisibleUpdate action state = case action of
  ShowCommentEditor curSelection                                                  -> EditorIsVisible curSelection
  UpdateSelection (RangeSelected range _) HT.CommentToolbarExtensionWithSelection -> EditorIsVisible (Just range)
  HideCommentEditor                                                               -> EditorIsHidden
  _ -> state

highlightedMarkAndBubbleUpdate :: ContributionAction -> Maybe (ID Void) -> Maybe (ID Void)
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
