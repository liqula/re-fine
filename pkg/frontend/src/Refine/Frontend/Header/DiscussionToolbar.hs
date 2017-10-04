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
  div_ ["className" $= "editorpage-header c_bg_note_bubble"] $ do
    div_ ["className" $= "editorpage-header__column1"] $ do
      div_ ["className" $= "left-column"] $ do

        div_ ["className" $= "editorpage-header__ibutton"] $ do
          ibutton_
            $ emptyIbuttonProps (ButtonImageIcon Svg.Close ColorSchemaDiscussion) [DocumentAction UpdateDocumentStateView]
            & ibListKey .~ "cancel"

        div_ ["className" $= "editorpage-header__ibutton"] $ do
          let img = if props ^. discToolbarFlatView then DiscussionFlat else DiscussionTree
           in ibutton_ $ emptyIbuttonProps (ButtonImageIcon img ColorSchemaDiscussion) [HeaderAction ToggleDiscussionFlatView]
                & ibListKey .~ "structure"
                & ibSize .~ XXLarge

      div_ ["className" $= "right-column"] $ do
        pure ()

    div_ ["className" $= "editorpage-header__column2"] $ do
      div_ ["className" $= "left-column"] $ do

        div_ ["className" $= "editorpage-header__vertical-bar"] ""
        case props ^. discToolbarDiscussionID of
          Nothing -> mempty
          Just did -> do
            let voteAction v = LoginGuardStash
                  [ContributionAction $ ToggleVoteOnContribution (ContribIDDiscussion (props ^. discToolbarIsNote) did) v]

            div_ ["className" $= "editorpage-header__ibutton"] $ do
              ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VotePositive ColorSchemaDiscussion) [voteAction Yeay]
                & ibListKey .~ "voteup"
                & ibIndexNum .~ Map.lookup Yeay (props ^. discToolbarPropsVotes)
                & ibSize .~ XXLarge

            div_ ["className" $= "editorpage-header__ibutton"] $ do
              ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VoteNegative ColorSchemaDiscussion) [voteAction Nay]
                & ibListKey .~ "votedown"
                & ibIndexNum .~ Map.lookup Nay (props ^. discToolbarPropsVotes)
                & ibSize .~ XXLarge

      div_ ["className" $= "right-column"] $ do
        pure ()

    div_ ["className" $= "editorpage-header__column3"] $ do
      div_ ["className" $= "left-column"] $ do

        div_ ["className" $= "editorpage-header__vertical-bar"] ""

      div_ ["className" $= "right-column"] $ do

        ibutton_ $ emptyIbuttonProps (ButtonImageIcon ArrowUp ColorSchemaDiscussion) [HeaderAction ScrollToPageTop]
          & ibListKey .~ "top"
          & ibSize    .~ XXLarge
