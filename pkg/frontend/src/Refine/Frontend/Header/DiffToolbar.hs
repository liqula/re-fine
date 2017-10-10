{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.DiffToolbar where
#include "import_frontend.hs"

import Refine.Common.Types
import Refine.Frontend.Access
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon
import Refine.Frontend.Icon.Svg as Svg
import Refine.Frontend.Store.Types


diffToolbar_ :: HasCallStack => DiffToolbarProps -> ReactElementM eventHandler ()
diffToolbar_ props = do

  ibutton_ $ emptyIbuttonProps
    (ButtonImageIcon Svg.IndexDesktop ColorSchemaDark)
    [HeaderAction ToggleIndexToolbarExtension]
    & ibSize .~ XXLarge

  div_ ["className" $= "main-content__header c-toolbar-sticky"] $ do
    div_ ["className" $= "main-content__header-inner fisx-css-toolbar-flex c-vdoc-toolbar"] $ do
      let EditIndex alledits thisedit = props ^. diffToolbarIndex
      span_ . cs $ "Edit " <> show (thisedit + 1) <> " of " <> show alledits
                           <> ", " <> show (props ^. diffToolbarEditKind)

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Close ColorSchemaDark) [ContributionAction HideContributionDialog]
        & ibListKey .~ "0"
        & ibSize .~ XXLarge

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      let voteAction v = LoginGuardStash
            [ContributionAction $ ToggleVoteOnContribution
                                    (ContribIDEdit $ props ^. diffToolbarPropsEditID) v]

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VotePositive ColorSchemaDark) [voteAction Yeay]
        & ibListKey .~ "1"
        & ibIndexNum .~ Map.lookup Yeay (props ^. diffToolbarPropsVotes)
        & ibSize .~ XXLarge

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VoteNegative ColorSchemaDark) [voteAction Nay]
        & ibListKey .~ "2"
        & ibIndexNum .~ Map.lookup Nay (props ^. diffToolbarPropsVotes)
        & ibSize .~ XXLarge

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon ArrowUp ColorSchemaDark) [HeaderAction ScrollToPageTop]
        & ibListKey .~ "3"
        & ibSize .~ XXLarge

      let img = if props ^. diffToolbarCollapsed then Svg.DiffExpand else Svg.DiffCollapse
       in ibutton_ $ emptyIbuttonProps (ButtonImageIcon img ColorSchemaDark) [DocumentAction ToggleCollapseDiff]
            & ibListKey .~ "4"
            & ibSize .~ XXLarge

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      when (props ^. diffToolbarEditable) $ do
        ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Edit ColorSchemaDark) [HeaderAction StartEdit]
          & ibListKey .~ "5"
          & ibSize .~ XXLarge

        div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.DiffDetails ColorSchemaDark) [ShowNotImplementedYet]
        & ibListKey .~ "6"
        & ibSize .~ XXLarge

    div_ ["className" $= "main-content__header-inner fisx-css-toolbar-flex c-vdoc-toolbar"] $ do
      let EditIndex alledits thisedit = props ^. diffToolbarIndex
      span_ . cs $ "Edit " <> show (thisedit + 1) <> " of " <> show alledits
                           <> ", " <> show (props ^. diffToolbarEditKind)
