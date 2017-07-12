{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.DiffToolbar where

import Refine.Frontend.Prelude

import qualified Data.Map as Map

import Refine.Common.Types
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon
import Refine.Frontend.Store.Types


diffToolbar_ :: HasCallStack => DiffToolbarProps -> ReactElementM eventHandler ()
diffToolbar_ props = do
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

      voteAction v = LoginGuardStash [ContributionAction $ ToggleVoteOnContribution (props ^. diffToolbarPropsEditID) v]

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

  ibutton_ $ emptyIbuttonProps "Toggle_collapse_diff" [DocumentAction ToggleCollapseDiff]
    & ibListKey .~ "4"
    & ibLabel .~ (if props ^. diffToolbarCollapsed then "expand" else "collapse")
    & ibSize .~ XXLarge

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  ibutton_ $ emptyIbuttonProps "Diff_details" [ShowNotImplementedYet]
    & ibListKey .~ "5"
    & ibLabel .~ "details"
    & ibSize .~ XXLarge
