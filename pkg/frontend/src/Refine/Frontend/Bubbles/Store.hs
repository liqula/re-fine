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

import Refine.Common.Types
import Refine.Frontend.Bubbles.Types
import Refine.Frontend.Types

bubblesStateUpdate :: RefineAction -> BubblesState -> BubblesState
bubblesStateUpdate action state =
  let newState = state
                  & bsCurrentSelection         %~ currentSelectionUpdate action
                  & bsCommentCategory          %~ commentCategoryUpdate action
                  & bsDiscussionIsVisible      %~ discussionIsVisibleUpdate action
                  & bsNoteIsVisible            %~ noteIsVisibleUpdate action
                  & bsCommentEditorIsVisible   %~ commentEditorIsVisibleUpdate action
                  & bsMarkPositions            %~ markPositionsUpdate action
  in newState

---------------------------------------------------------------------------

currentSelectionUpdate :: RefineAction -> Selection -> Selection
currentSelectionUpdate action state = case action of
  BubblesAction (UpdateSelection newState) -> newState
  BubblesAction ClearSelection -> (Nothing, Nothing)
  BubblesAction SubmitEdit     -> (Nothing, Nothing)
  _ -> state

commentCategoryUpdate :: RefineAction -> Maybe CommentCategory -> Maybe CommentCategory
commentCategoryUpdate action state = case action of
  BubblesAction (SetCommentCategory category) -> Just category
  BubblesAction HideCommentEditor -> Nothing -- when closing the comment editor, reset the selection
  _ -> state

discussionIsVisibleUpdate :: RefineAction -> Maybe (ID Discussion) -> Maybe (ID Discussion)
discussionIsVisibleUpdate action state = case action of
  BubblesAction (ShowDiscussionOverlay discussionId) -> Just discussionId
  BubblesAction HideCommentOverlay -> Nothing
  _ -> state

noteIsVisibleUpdate :: RefineAction -> Maybe (ID Note) -> Maybe (ID Note)
noteIsVisibleUpdate action state = case action of
  BubblesAction (ShowNoteOverlay noteId) -> Just noteId
  BubblesAction HideCommentOverlay -> Nothing
  _ -> state

commentEditorIsVisibleUpdate :: RefineAction -> (Bool, Maybe Range) -> (Bool, Maybe Range)
commentEditorIsVisibleUpdate action state = case action of
  BubblesAction (ShowCommentEditor curSelection) -> (True, curSelection)
  BubblesAction HideCommentEditor -> (False, Nothing)
  _ -> state

markPositionsUpdate :: RefineAction -> MarkPositions -> MarkPositions
markPositionsUpdate action state = case action of
    BubblesAction (AddMarkPosition dataChunkId pos scroll) -> MarkPositions $ M.alter (\_ -> Just (pos, scroll)) dataChunkId (_unMarkPositions state)
    _ -> state

