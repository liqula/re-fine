{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.DiffToolbar where
#include "import_frontend.hs"

import Refine.Common.Types
import Refine.Frontend.Access
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon
import Refine.Frontend.Store.Types


diffToolbar_ :: HasCallStack => DiffToolbarProps -> ReactElementM eventHandler ()
diffToolbar_ props = do

  iconButton_ $ defaultIconButtonProps @[GlobalAction]
    & iconButtonPropsListKey      .~ "index"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Index_desktop", "dark") XXLarge
    & iconButtonPropsElementName  .~ "btn-index"
    & iconButtonPropsLabel        .~ "index"
    & iconButtonPropsOnClick      .~ [HeaderAction ToggleIndexToolbarExtension]

  let EditIndex alledits thisedit = props ^. diffToolbarIndex
  span_ . cs $ "Edit " <> show (thisedit + 1) <> " of " <> show alledits
                       <> ", " <> show (props ^. diffToolbarEditKind)

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  ibutton_ $ emptyIbuttonProps "Close" [ContributionAction HideContributionDialog]
    & ibListKey .~ "0"
    & ibLabel .~ "back"
    & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  let voteButtonLabel :: Vote -> ST
      voteButtonLabel = \case
        Yeay -> "up ("   <> count Yeay <> ")"
        Nay  -> "down (" <> count Nay  <> ")"
        where
          count v = cs . show . fromMaybe 0 $ Map.lookup v vs
          vs = props ^. diffToolbarPropsVotes

      voteAction v = LoginGuardStash
        [ContributionAction $ ToggleVoteOnContribution
                                (ContribIDEdit $ props ^. diffToolbarPropsEditID) v]

  ibutton_ $ emptyIbuttonProps "Vote_positive" [voteAction Yeay]
    & ibListKey .~ "1"
    & ibLabel .~ voteButtonLabel Yeay
    & ibSize .~ XXLarge

  ibutton_ $ emptyIbuttonProps "Vote_negative" [voteAction Nay]
    & ibListKey .~ "2"
    & ibLabel .~ voteButtonLabel Nay
    & ibSize .~ XXLarge

  ibutton_ $ emptyIbuttonProps "Arrow_up" [HeaderAction ScrollToPageTop]
    & ibListKey .~ "3"
    & ibLabel .~ "motivation"
    & ibSize .~ XXLarge

  let collapseOrExpand = if props ^. diffToolbarCollapsed then "expand" else "collapse"
  ibutton_ $ emptyIbuttonProps ("Toggle_" <> collapseOrExpand <> "_diff") [DocumentAction ToggleCollapseDiff]
    & ibListKey .~ "4"
    & ibLabel .~ collapseOrExpand
    & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  when (props ^. diffToolbarEditable) $ do
    ibutton_ $ emptyIbuttonProps "Update_edit" [HeaderAction StartEdit]
      & ibListKey .~ "5"
      & ibLabel .~ "update"
      & ibSize .~ XXLarge

    div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  ibutton_ $ emptyIbuttonProps "Diff_details" [ShowNotImplementedYet]
    & ibListKey .~ "6"
    & ibLabel .~ "details"
    & ibSize .~ XXLarge
