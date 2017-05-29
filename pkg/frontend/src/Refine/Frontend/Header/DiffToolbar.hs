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

import Refine.Frontend.Contribution.Types
import Refine.Frontend.Icon
import Refine.Frontend.Store.Types


diffToolbar :: View '[]
diffToolbar = mkView "DiffToolbar" $ do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          ibutton_ $ emptyIbuttonProps "Close" [ContributionAction HideContributionDialog]
            & ibListKey .~ "0"
            & ibLabel .~ "back"
            & ibSize .~ XXLarge

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          ibutton_ $ emptyIbuttonProps "Vote_positive" []
            & ibListKey .~ "1"
            & ibLabel .~ "up"
            & ibSize .~ XXLarge

          ibutton_ $ emptyIbuttonProps "Vote_negative" []
            & ibListKey .~ "2"
            & ibLabel .~ "down"
            & ibSize .~ XXLarge

          ibutton_ $ emptyIbuttonProps "Arrow_up" []
            & ibListKey .~ "3"
            & ibLabel .~ "motivation"
            & ibSize .~ XXLarge

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          ibutton_ $ emptyIbuttonProps "Diff_details" []
            & ibListKey .~ "4"
            & ibLabel .~ "details"
            & ibSize .~ XXLarge

diffToolbar_ :: ReactElementM eventHandler ()
diffToolbar_ = view_ diffToolbar "diffToolbar_"
