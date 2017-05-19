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

module Refine.Frontend.Header.EditToolbar where

import Refine.Frontend.Prelude

import           Refine.Frontend.Document.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Store.Types

editToolbar :: View '[]
editToolbar = mkView "EditToolbar" $ do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          let editButton = def @IconButtonProps
                & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL
                & iconButtonPropsElementName  .~ "btn-index"

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "h1"
            & iconButtonPropsLabel        .~ "header 1"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "h2"
            & iconButtonPropsLabel        .~ "header 2"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "h3"
            & iconButtonPropsLabel        .~ "header 3"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "bold"
            & iconButtonPropsLabel        .~ "bold"
            & iconButtonPropsOnClick      .~ [DocumentAction DocumentToggleBold]

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "italic"
            & iconButtonPropsLabel        .~ "italic"
            & iconButtonPropsOnClick      .~ [DocumentAction DocumentToggleItalic]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "bullets"
            & iconButtonPropsLabel        .~ "bullets"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton
            & iconButtonPropsListKey      .~ "numbers"
            & iconButtonPropsLabel        .~ "numbers"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXL
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsOnClick      .~ [DocumentAction RequestDocumentSave]

editToolbar_ :: ReactElementM eventHandler ()
editToolbar_ = view_ editToolbar "editToolbar_"
