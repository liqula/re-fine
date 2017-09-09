{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Contribution.Store where
#include "import_frontend.hs"

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types


contributionStateUpdate :: HasCallStack => GlobalAction -> ContributionState -> ContributionState
contributionStateUpdate a = localAction a . globalAction a
  where
    localAction (ContributionAction act) st = st
      & csCurrentSelectionWithPx   %~ currentRangeUpdate act
      & csDisplayedContributionID  %~ displayedContributionUpdate act
      & csHighlightedMarkAndBubble %~ highlightedMarkAndBubbleUpdate act
      & csAllVerticalSpanBounds    %~ allVerticalSpanBoundsUpdate act
      & csBubblePositioning        %~ bubblePositioningUpdate act
      & csBubbleFilter             %~ bubbleFilterUpdate act
    localAction _ st = st

    globalAction act st = st
      & csQuickCreateShowState     %~ quickCreateShowStateUpdate act
      & csActiveDialog             %~ activeDialogUpdate act


currentRangeUpdate :: HasCallStack => ContributionAction -> Maybe SelectionStateWithPx -> Maybe SelectionStateWithPx
currentRangeUpdate act = case act of
  SetRange range -> const (Just range)
  ClearRange     -> const Nothing
  _ -> id

displayedContributionUpdate :: HasCallStack => ContributionAction -> Maybe ContributionID -> Maybe ContributionID
displayedContributionUpdate act st = case act of
  ShowContributionDialog cid'
    | st == Just cid' -> Nothing
    | otherwise       -> Just cid'
  HideContributionDialog -> Nothing
  _ -> st

activeDialogUpdateForShowCommentEditor :: b{-to prevent let-floating-} -> Maybe ActiveDialog
activeDialogUpdateForShowCommentEditor = Just . ActiveDialogComment .
  newLocalStateRef (CommentInputState (CommentInfo "" Nothing) False False)

activeDialogUpdate :: HasCallStack => GlobalAction -> Maybe ActiveDialog -> Maybe ActiveDialog
activeDialogUpdate = \case
  ContributionAction ShowCommentEditor                            -> activeDialogUpdateForShowCommentEditor
  ContributionAction HideCommentEditor                            -> const Nothing
  DocumentAction (DocumentSave (FormBegin (EditIsNotInitial, e))) -> const . Just $ ActiveDialogEdit e
  DocumentAction (DocumentSave (FormComplete _))                  -> const Nothing
  DocumentAction DocumentCancelSave                               -> const Nothing
  _ -> id

highlightedMarkAndBubbleUpdate :: HasCallStack => ContributionAction -> [MarkID] -> [MarkID]
highlightedMarkAndBubbleUpdate (HighlightMarkAndBubble cids) _    = cids
highlightedMarkAndBubbleUpdate _                             cids = cids

quickCreateShowStateUpdate :: HasCallStack => GlobalAction -> QuickCreateShowState -> QuickCreateShowState
quickCreateShowStateUpdate act st = case act of
  ContributionAction (SetRange _)               -> somethingWasSelected
  ContributionAction ClearRange                 -> selectionWasRemoved
  HeaderAction ToggleCommentToolbarExtension    -> toolbarWasToggled
  HeaderAction StartTextSpecificComment         -> QuickCreateBlocked
  HeaderAction StartEdit                        -> QuickCreateNotShown  -- (article is hidden, so
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

allVerticalSpanBoundsUpdate :: HasCallStack => ContributionAction -> AllVerticalSpanBounds -> AllVerticalSpanBounds
allVerticalSpanBoundsUpdate (SetAllVerticalSpanBounds positions) = allVerticalSpanBounds .~ Map.fromList positions
allVerticalSpanBoundsUpdate _ = id

bubblePositioningUpdate :: HasCallStack => ContributionAction -> BubblePositioning -> BubblePositioning
bubblePositioningUpdate (SetBubblePositioning strategy) _ = strategy
bubblePositioningUpdate _ st = st

bubbleFilterUpdate :: HasCallStack => ContributionAction -> Maybe (Set ContributionID) -> Maybe (Set ContributionID)
bubbleFilterUpdate (SetBubbleFilter f) _ = f
bubbleFilterUpdate _ st = st
