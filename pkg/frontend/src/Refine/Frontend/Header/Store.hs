{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.Store
  ( headerStateUpdate
  ) where
#include "import_frontend.hs"

import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Store.Types


headerStateUpdate :: HasCallStack => GlobalAction -> HeaderState -> HeaderState
headerStateUpdate act st = st
  & hsReadOnly               %~ readOnlyUpdate act
  & hsToolbarExtensionStatus %~ toolbarExtensionUpdate act
  & hsDiscussionFlatView     %~ discussionFlatViewUpdate act

readOnlyUpdate :: HasCallStack => GlobalAction -> Bool -> Bool
readOnlyUpdate (HeaderAction ToggleReadOnly) = not
readOnlyUpdate _                             = id

discussionFlatViewUpdate :: HasCallStack => GlobalAction -> Bool -> Bool
discussionFlatViewUpdate (HeaderAction ToggleDiscussionFlatView) = not
discussionFlatViewUpdate _                                       = id

toolbarExtensionUpdate :: HasCallStack => GlobalAction -> ToolbarExtensionStatus -> ToolbarExtensionStatus
toolbarExtensionUpdate act st = case (st, act) of
    (ToolbarExtensionClosed,               HeaderAction ToggleIndexToolbarExtension)   -> IndexToolbarExtension
    (IndexToolbarExtension,                HeaderAction ToggleIndexToolbarExtension)   -> ToolbarExtensionClosed
    (IndexToolbarExtension,                DocumentAction ToggleCollapseDiff)          -> ToolbarExtensionClosed
    (IndexToolbarExtension,                HeaderAction StartEdit)                     -> ToolbarExtensionClosed
    (IndexToolbarExtension,                ContributionAction ShowContributionDialog{})-> ToolbarExtensionClosed
    (IndexToolbarExtension,                ContributionAction HideContributionDialog)  -> ToolbarExtensionClosed
    (_,                                    HeaderAction ScrollToBlockKey{})            -> ToolbarExtensionClosed

    (ToolbarExtensionClosed,               HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange
    (CommentToolbarExtensionWithoutRange,  ContributionAction ShowCommentEditor)       -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithoutRange,  HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithRange,     ContributionAction ShowCommentEditor)       -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithRange,     HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed

    (CommentToolbarExtensionWithoutRange,  HeaderAction StartTextSpecificComment)      -> CommentToolbarExtensionWithRange
    (_,                                    HeaderAction StartTextSpecificComment)      -> bad1

    (ToolbarExtensionClosed,               HeaderAction (OpenEditToolbarLinkEditor l)) -> EditToolbarLinkEditor l
    (_,                                    HeaderAction (OpenEditToolbarLinkEditor _)) -> ToolbarExtensionClosed

    (_,                                    HeaderAction CloseToolbarExtension)         -> ToolbarExtensionClosed

    _ -> st
  where
    bad1 = error $ "text-specific comment cannot start when toolbar extension is closed or in selection mode: " <> show (act, st)
