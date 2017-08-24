{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.DiscussionToolbar where

import           Refine.Frontend.Prelude

import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Document.Types

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

  ibutton_ $ emptyIbuttonProps "Arrow_up" [HeaderAction ScrollToPageTop]
    & ibListKey .~ "3"
    & ibLabel   .~ "top"
    & ibSize    .~ XXLarge
