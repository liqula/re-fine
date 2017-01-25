{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Refine.Frontend.Bubbles.Store where


import           Control.Lens ((&), (%~))

import Refine.Frontend.Bubbles.Types
import Refine.Frontend.Types

bubblesStateUpdate :: RefineAction -> BubblesState -> BubblesState
bubblesStateUpdate action state =
  let newState = state
                  & bsCurrentSelection         %~ currentSelectionUpdate action
                  & bsCommentIsVisible         %~ commentIsVisibleUpdate action
                  & bsCommentEditorIsVisible   %~ commentEditorIsVisibleUpdate action
  in newState

---------------------------------------------------------------------------

currentSelectionUpdate :: RefineAction -> Selection -> Selection
currentSelectionUpdate action state = case action of
    UpdateSelection newState -> newState
    ClearSelection -> (Nothing, Nothing)
    SubmitPatch    -> (Nothing, Nothing)
    _ -> state

commentIsVisibleUpdate :: RefineAction -> Bool -> Bool
commentIsVisibleUpdate action state = case action of
    ShowComment -> True
    HideComment -> False
    _ -> state

commentEditorIsVisibleUpdate :: RefineAction -> (Bool, Maybe Range) -> (Bool, Maybe Range)
commentEditorIsVisibleUpdate action state = case action of
    ShowCommentEditor curSelection -> (True, curSelection)
    HideCommentEditor -> (False, Nothing)
    _ -> state

