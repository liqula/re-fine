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

module Refine.Frontend.Header.Toolbar where

import           Control.Lens hiding (children)
import           Data.Default (def)
import           Data.String.Conversions
import           React.Flux

import           Refine.Common.Types ( EditKind(..) )
import qualified Refine.Frontend.Header.Types as RS
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types

toolbar :: View '[()]
toolbar = mkView "Toolbar" $ \() ->
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          let toolbarButton = def

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "index"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Index_desktop", "dark") XXL
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "index"
            & iconButtonPropsClickActions .~ [RS.ShowNotImplementedYet]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-comment"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXL
            & iconButtonPropsElementName  .~ "btn-add-annotation"
            & iconButtonPropsLabel        .~ "new comment"
            & iconButtonPropsClickActions .~ [RS.HeaderAction RS.ToggleCommentToolbarExtension]
            & iconButtonPropsClickPropag  .~ False

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-edit"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXL
            & iconButtonPropsElementName  .~ "bt-add-modification"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "new edit"
            & iconButtonPropsClickActions .~ [RS.HeaderAction RS.ToggleEditToolbarExtension]
            & iconButtonPropsClickPropag  .~ False

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-comments"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" False ("icon-Comment", "dark") XXL
            & iconButtonPropsElementName  .~ "all-annotations"   -- RENAME: annotation => comment
            & iconButtonPropsLabel        .~ "all comments"
            & iconButtonPropsClickActions .~ [RS.ShowNotImplementedYet]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-edits"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXL
            & iconButtonPropsElementName  .~ "all-modifications"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "all edits"
            & iconButtonPropsClickActions .~ [RS.ShowNotImplementedYet]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "read-only"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Reader", "bright") XXL
            & iconButtonPropsElementName  .~ "btn-read-mode"
            & iconButtonPropsLabel        .~ "read mode"
            & iconButtonPropsClickActions .~ [RS.ShowNotImplementedYet]
            & iconButtonPropsAlignRight   .~ True

toolbar_ :: ReactElementM eventHandler ()
toolbar_ = view_ toolbar "toolbar_" ()


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: RS.ToolbarExtensionStatus
  }
  deriving (Eq)

commentToolbarExtension :: View '[CommentToolbarExtensionProps]
commentToolbarExtension = mkView "CommentToolbarExtension" $ \case
  (CommentToolbarExtensionProps RS.CommentToolbarExtensionWithSelection) -> frame $ do
    div_ "Please select the text you would like to comment on"
  (CommentToolbarExtensionProps RS.CommentToolbarExtensionWithButtons) -> frame $ do
    iconButton_ $ def
      & iconButtonPropsListKey      .~ "comment-range"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L
      & iconButtonPropsElementName  .~ "btn-new-ann-text" -- RENAME: ann => comment
      & iconButtonPropsLabel        .~ "text-specific comment"
      & iconButtonPropsClickActions .~ [RS.HeaderAction RS.StartTextSpecificComment]
      & iconButtonPropsClickPropag  .~ False
    iconButton_ $ def
      & iconButtonPropsListKey      .~ "comment-all"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L
      & iconButtonPropsElementName  .~ "btn-new-ann-doc"  -- RENAME: ann => comment
      & iconButtonPropsLabel        .~ "general comment"
      & iconButtonPropsClickActions .~ [RS.ShowNotImplementedYet]
  (CommentToolbarExtensionProps _) -> mempty
  where
    frame :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
    frame children = div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ [classNames [ ("c-vdoc-toolbar-extension__annotation", True)   -- RENAME: annotation => comment
                           , ("c-vdoc-toolbar-extension--expanded", True) ]
               ]
            children


commentToolbarExtension_ :: CommentToolbarExtensionProps -> ReactElementM eventHandler ()
commentToolbarExtension_ !props = view_ commentToolbarExtension "commentToolbarExtension_" props


newtype EditToolbarExtensionProps = EditToolbarExtensionProps
  { _etepStatus :: RS.ToolbarExtensionStatus
  }
  deriving (Eq)

editToolbarExtension :: View '[EditToolbarExtensionProps]
editToolbarExtension = mkView "EditToolbarExtension" $ \case
  (EditToolbarExtensionProps RS.EditToolbarExtension) -> do
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
      iconButton_ $ def
        & iconButtonPropsListKey      .~ cs (show kind)
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-New_Edit", "dark") L
        & iconButtonPropsElementName  .~ "btn-new-mod-text" -- RENAME: mod => edit
        & iconButtonPropsLabel        .~ cs (show kind)
        & iconButtonPropsClickActions .~ [RS.HeaderAction (RS.StartEdit kind)]
        & iconButtonPropsClickPropag  .~ False

editToolbarExtension_ :: EditToolbarExtensionProps -> ReactElementM eventHandler ()
editToolbarExtension_ !props = view_ editToolbarExtension "editToolbarExtension_" props
