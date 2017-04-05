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

module Refine.Frontend.Header.Toolbar where

import           Control.Lens hiding (children)
import           Data.Default (def)
import           Data.String.Conversions
import           React.Flux

import           Refine.Common.Types ( EditKind(..) )
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types

toolbar :: View '[]
toolbar = mkView "Toolbar" $ do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          let toolbarButton = def @IconButtonProps

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "index"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Index_desktop", "dark") XXL
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "index"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-comment"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXL
            & iconButtonPropsElementName  .~ "btn-add-annotation"  -- RENAME
            & iconButtonPropsLabel        .~ "new comment"
            & iconButtonPropsOnClick      .~ [HeaderAction ToggleCommentToolbarExtension]
            & iconButtonPropsClickPropag  .~ False

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-edit"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXL
            & iconButtonPropsElementName  .~ "bt-add-modification"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "new edit"
            & iconButtonPropsOnClick      .~ [HeaderAction ToggleEditToolbarExtension]
            & iconButtonPropsClickPropag  .~ False

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-comments"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" False ("icon-Comment", "dark") XXL
            & iconButtonPropsElementName  .~ "all-annotations"   -- RENAME: annotation => comment
            & iconButtonPropsLabel        .~ "all comments"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-edits"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXL
            & iconButtonPropsElementName  .~ "all-modifications"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "all edits"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "read-only"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Reader", "bright") XXL
            & iconButtonPropsElementName  .~ "btn-read-mode"
            & iconButtonPropsLabel        .~ "read mode"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]
            & iconButtonPropsAlignRight   .~ True

toolbar_ :: ReactElementM eventHandler ()
toolbar_ = view_ toolbar "toolbar_"


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: ToolbarExtensionStatus
  }
  deriving (Eq)

commentToolbarExtension :: View '[CommentToolbarExtensionProps]
commentToolbarExtension = mkView "CommentToolbarExtension" $ \case
  (CommentToolbarExtensionProps CommentToolbarExtensionWithRange) -> frame $ do
    div_ "Please select the text you would like to comment on"
  (CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange) -> frame $ do
    iconButton_ $ def @IconButtonProps
      & iconButtonPropsListKey      .~ "comment-range"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L
      & iconButtonPropsElementName  .~ "btn-new-ann-text" -- RENAME: ann => comment
      & iconButtonPropsLabel        .~ "text-specific comment"
      & iconButtonPropsOnClick      .~ [HeaderAction StartTextSpecificComment]
      & iconButtonPropsClickPropag  .~ False
    iconButton_ $ def @IconButtonProps
      & iconButtonPropsListKey      .~ "comment-all"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L
      & iconButtonPropsElementName  .~ "btn-new-ann-doc"  -- RENAME: ann => comment
      & iconButtonPropsLabel        .~ "general comment"
      & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]
  (CommentToolbarExtensionProps _) -> mempty
  where
    frame :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
    frame children = div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ [classNamesAny
                           [ ("c-vdoc-toolbar-extension__annotation", True)   -- RENAME: annotation => comment
                           , ("c-vdoc-toolbar-extension--expanded", True) ]
               ]
            children


commentToolbarExtension_ :: CommentToolbarExtensionProps -> ReactElementM eventHandler ()
commentToolbarExtension_ !props = view_ commentToolbarExtension "commentToolbarExtension_" props


newtype EditToolbarExtensionProps = EditToolbarExtensionProps
  { _etepStatus :: ToolbarExtensionStatus
  }
  deriving (Eq)

editToolbarExtension :: View '[EditToolbarExtensionProps]
editToolbarExtension = mkView "EditToolbarExtension" $ \case
  (EditToolbarExtensionProps EditToolbarExtension) -> do
    div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)
            editButton `mapM_` [Grammar, Phrasing, Meaning]

  (EditToolbarExtensionProps _) -> mempty
  where
    editButton :: EditKind -> ReactElementM eventHandler ()
    editButton kind =
      iconButton_ $ def @IconButtonProps
        & iconButtonPropsListKey      .~ cs (show kind)
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-New_Edit", "dark") L
        & iconButtonPropsElementName  .~ "btn-new-mod-text" -- RENAME: mod => edit
        & iconButtonPropsLabel        .~ cs (show kind)
        & iconButtonPropsOnClick      .~ [HeaderAction (StartEdit kind)]
        & iconButtonPropsClickPropag  .~ False

editToolbarExtension_ :: EditToolbarExtensionProps -> ReactElementM eventHandler ()
editToolbarExtension_ !props = view_ editToolbarExtension "editToolbarExtension_" props
