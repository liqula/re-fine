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
import           Refine.Frontend.Store.Types

editToolbar :: View '[]
editToolbar = mkView "EditToolbar" $ do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          let editButton icon = defaultIconButtonProps @[GlobalAction]
                & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-" <> icon, "dark") XXLarge
                & iconButtonPropsElementName  .~ "btn-index"

          iconButton_ $ editButton "Edit_toolbar_h1"
            & iconButtonPropsListKey      .~ "h1"
            & iconButtonPropsLabel        .~ "header 1"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton "Edit_toolbar_h2"
            & iconButtonPropsListKey      .~ "h2"
            & iconButtonPropsLabel        .~ "header 2"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton "Edit_toolbar_h3"
            & iconButtonPropsListKey      .~ "h3"
            & iconButtonPropsLabel        .~ "header 3"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ editButton "Edit_toolbar_bold"
            & iconButtonPropsListKey      .~ "bold"
            & iconButtonPropsLabel        .~ "bold"
            & iconButtonPropsOnClick      .~ [DocumentAction DocumentToggleBold]

          iconButton_ $ editButton "Edit_toolbar_italic"
            & iconButtonPropsListKey      .~ "italic"
            & iconButtonPropsLabel        .~ "italic"
            & iconButtonPropsOnClick      .~ [DocumentAction DocumentToggleItalic]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ editButton "Edit_toolbar_bullets"
            & iconButtonPropsListKey      .~ "bullets"
            & iconButtonPropsLabel        .~ "bullets"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton "Edit_toolbar_numbers"
            & iconButtonPropsListKey      .~ "numbers"
            & iconButtonPropsLabel        .~ "numbers"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ editButton mempty
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXLarge
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsOnClick      .~ [DocumentAction RequestDocumentSave]

editToolbar_ :: ReactElementM eventHandler ()
editToolbar_ = view_ editToolbar "editToolbar_"
