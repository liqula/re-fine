{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.DiscussionToolbar where
#include "import_frontend.hs"

import           Refine.Common.Types
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Store.Types

-- FUTUREWORK: this should probably be a component, but if we do the obvious minimal change to
-- introduce a @View '[]@, the styling breaks completely.  note that this does not fix #376 either.
discussionToolbar_ :: HasCallStack => DiscussionToolbarProps -> ReactElementM eventHandler ()
discussionToolbar_ props = do
  ibutton_
    $ emptyIbuttonProps "Close" [DocumentAction UpdateDocumentStateView]
    & ibListKey        .~ "cancel"
    & ibSize           .~ Large

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  let collapseOrExpand = if props ^. discToolbarFlatView then "expand" else "collapse"
  ibutton_ $ emptyIbuttonProps ("Toggle_" <> collapseOrExpand <> "_diff") [HeaderAction ToggleDiscussionFlatView]
    & ibListKey .~ "4"
    & ibLabel .~ (if props ^. discToolbarFlatView then "Tree view" else "Flat view")
    & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  case props ^. discToolbarDiscussionID of
    Nothing -> mempty
    Just did -> do
      let voteButtonLabel :: Vote -> ST
          voteButtonLabel = \case
            Yeay -> "up ("   <> count Yeay <> ")"
            Nay  -> "down (" <> count Nay  <> ")"
            where
              count v = cs . show . fromMaybe 0 $ Map.lookup v vs
              vs = props ^. discToolbarPropsVotes

          voteAction v = LoginGuardStash
            [ContributionAction $ ToggleVoteOnContribution (ContribIDDiscussion (props ^. discToolbarIsNote) did) v]

      ibutton_ $ emptyIbuttonProps "Vote_positive" [voteAction Yeay]
        & ibListKey .~ "1"
        & ibLabel .~ voteButtonLabel Yeay
        & ibSize .~ XXLarge

      ibutton_ $ emptyIbuttonProps "Vote_negative" [voteAction Nay]
        & ibListKey .~ "2"
        & ibLabel .~ voteButtonLabel Nay
        & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  ibutton_ $ emptyIbuttonProps "Arrow_up" [HeaderAction ScrollToPageTop]
    & ibListKey .~ "3"
    & ibLabel   .~ "top"
    & ibSize    .~ XXLarge
