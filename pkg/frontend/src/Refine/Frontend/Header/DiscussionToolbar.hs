{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.DiscussionToolbar where
#include "import_frontend.hs"

import           Refine.Common.Types
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Store.Types

-- | FUTUREWORK: this should probably be a component, but if we do the obvious minimal change to
-- introduce a @View '[]@, the styling breaks completely.  note that this does not fix #376 either.
discussionToolbar_ :: HasCallStack => DiscussionToolbarProps -> ReactElementM eventHandler ()
discussionToolbar_ props = do
  ibutton_
    $ emptyIbuttonProps (ButtonImageIcon Svg.Close ColorSchemaDiscussion) [DocumentAction UpdateDocumentStateView]
    & ibListKey .~ "cancel"

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  let img = if props ^. discToolbarFlatView then DiscussionFlat else DiscussionTree
   in ibutton_ $ emptyIbuttonProps (ButtonImageIcon img ColorSchemaDiscussion) [HeaderAction ToggleDiscussionFlatView]
        & ibListKey .~ "structure"
        & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  case props ^. discToolbarDiscussionID of
    Nothing -> mempty
    Just did -> do
      let voteAction v = LoginGuardStash
            [ContributionAction $ ToggleVoteOnContribution (ContribIDDiscussion (props ^. discToolbarIsNote) did) v]

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VotePositive ColorSchemaDiscussion) [voteAction Yeay]
        & ibListKey .~ "voteup"
        & ibIndexNum .~ Map.lookup Yeay (props ^. discToolbarPropsVotes)
        & ibSize .~ XXLarge

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VoteNegative ColorSchemaDiscussion) [voteAction Nay]
        & ibListKey .~ "votedown"
        & ibIndexNum .~ Map.lookup Nay (props ^. discToolbarPropsVotes)
        & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  ibutton_ $ emptyIbuttonProps (ButtonImageIcon ArrowUp ColorSchemaDiscussion) [HeaderAction ScrollToPageTop]
    & ibListKey .~ "top"
    & ibSize    .~ XXLarge
