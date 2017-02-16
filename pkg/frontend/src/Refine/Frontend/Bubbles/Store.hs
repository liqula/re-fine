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

module Refine.Frontend.Bubbles.Store where

import           Control.Lens ((&), (%~))
import qualified Data.Map.Strict as M
import           Data.Void

import Refine.Common.Types
import Refine.Frontend.Bubbles.Types


bubblesStateUpdate :: BubblesAction -> BubblesState -> BubblesState
bubblesStateUpdate action state =
  let newState = state
                  & bsCurrentSelection         %~ currentSelectionUpdate action
                  & bsCommentCategory          %~ commentCategoryUpdate action
                  & bsDiscussionId      %~ discussionIsVisibleUpdate action
                  & bsNoteId            %~ noteIsVisibleUpdate action
                  & bsCommentEditorIsVisible   %~ commentEditorIsVisibleUpdate action
                  & bsHighlightedMarkAndBubble %~ highlightedMarkAndBubbleUpdate action
                  & bsMarkPositions            %~ markPositionsUpdate action
  in newState


---------------------------------------------------------------------------

currentSelectionUpdate :: BubblesAction -> Selection -> Selection
currentSelectionUpdate action state = case action of
  (UpdateSelection newState) -> newState
  ClearSelection -> (Nothing, Nothing)
  SubmitEdit     -> (Nothing, Nothing)
  _ -> state

commentCategoryUpdate :: BubblesAction -> Maybe CommentCategory -> Maybe CommentCategory
commentCategoryUpdate action state = case action of
  (SetCommentCategory category) -> Just category
  HideCommentEditor -> Nothing -- when closing the comment editor, reset the selection
  _ -> state

discussionIsVisibleUpdate :: BubblesAction -> Maybe (ID Discussion) -> Maybe (ID Discussion)
discussionIsVisibleUpdate action state = case action of
  (ShowDiscussionOverlay discussionId) -> Just discussionId
  HideCommentOverlay -> Nothing
  _ -> state

noteIsVisibleUpdate :: BubblesAction -> Maybe (ID Note) -> Maybe (ID Note)
noteIsVisibleUpdate action state = case action of
  (ShowNoteOverlay noteId) -> Just noteId
  HideCommentOverlay -> Nothing
  _ -> state

commentEditorIsVisibleUpdate :: BubblesAction -> (Bool, Maybe Range) -> (Bool, Maybe Range)
commentEditorIsVisibleUpdate action state = case action of
  (ShowCommentEditor curSelection) -> (True, curSelection)
  HideCommentEditor -> (False, Nothing)
  _ -> state

highlightedMarkAndBubbleUpdate :: BubblesAction -> Maybe (ID Void) -> Maybe (ID Void)
highlightedMarkAndBubbleUpdate action state = case action of
    (HighlightMarkAndBubble dataChunkId) -> Just dataChunkId
    UnhighlightMarkAndBubble -> Nothing
    _ -> state

markPositionsUpdate :: BubblesAction -> MarkPositions -> MarkPositions
markPositionsUpdate action state = case action of
    (AddMarkPosition dataChunkId topOffset scrollOffset)
      -> let upd old@(Just (oldTopOffset, _)) | topOffset >= oldTopOffset = old
             upd _ = Just (topOffset, scrollOffset)
         in MarkPositions $ M.alter upd dataChunkId (_unMarkPositions state)
    _ -> state
