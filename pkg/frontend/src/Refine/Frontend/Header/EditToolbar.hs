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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.EditToolbar where

import           Control.Lens
import           Data.Default (def)
import           React.Flux

import           Refine.Frontend.Store
import           Refine.Frontend.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.UtilityWidgets

editToolbar :: View '[()]  -- TODO: can we get rid of the ()?  (grep for '[()]', it also happens in one more case!)
editToolbar = mkView "EditToolbar" $ \() ->
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          let editButton = def
                & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL
                & iconButtonPropsElementName  .~ "btn-index"

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "h1"
            & iconButtonPropsLabel        .~ "header 1"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "h2"
            & iconButtonPropsLabel        .~ "header 2"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "h3"
            & iconButtonPropsLabel        .~ "header 3"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "bold"
            & iconButtonPropsLabel        .~ "bold"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "italic"
            & iconButtonPropsLabel        .~ "italic"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "bullets"
            & iconButtonPropsLabel        .~ "bullets"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "numbers"
            & iconButtonPropsLabel        .~ "numbers"
            & iconButtonPropsClickHandler .~ (\_ -> dispatch ShowNotImplementedYet)

          iconButton_ $ editButton
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXL
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsClickHandler .~ (\_ -> dispatch $ DocumentAction DocumentEditSave)

editToolbar_ :: ReactElementM eventHandler ()
editToolbar_ = view_ editToolbar "editToolbar_" ()
