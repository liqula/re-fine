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

module Refine.Frontend.Header.Toolbar where

import Refine.Frontend.Prelude

import           Refine.Common.Types ( EditKind(..) )
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Icon

toolbar :: View '[]
toolbar = mkView "Toolbar" $ do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          let toolbarButton = defaultIconButtonProps @[GlobalAction]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "index"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Index_desktop", "dark") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "index"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-comment"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXLarge
            & iconButtonPropsElementName  .~ "btn-add-annotation"  -- RENAME
            & iconButtonPropsLabel        .~ "new comment"
            & iconButtonPropsOnClick      .~ [HeaderAction ToggleCommentToolbarExtension]
            & iconButtonPropsClickPropag  .~ False

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-edit"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXLarge
            & iconButtonPropsElementName  .~ "bt-add-modification"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "new edit"
            & iconButtonPropsOnClick      .~ [HeaderAction ToggleEditToolbarExtension]
            & iconButtonPropsClickPropag  .~ False

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-comments"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Comment", "dark") XXLarge
            & iconButtonPropsElementName  .~ "all-annotations"   -- RENAME: annotation => comment
            & iconButtonPropsLabel        .~ "all comments"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-edits"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXLarge
            & iconButtonPropsElementName  .~ "all-modifications"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "all edits"
            & iconButtonPropsOnClick      .~ [ShowNotImplementedYet]

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "read-only"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Reader", "dark") XXLarge
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

instance UnoverlapAllEq CommentToolbarExtensionProps

commentToolbarExtension :: View '[CommentToolbarExtensionProps]
commentToolbarExtension = mkView "CommentToolbarExtension" $ \case
  (CommentToolbarExtensionProps CommentToolbarExtensionWithRange) -> frame $ do
    div_ "Please select the text you would like to comment on"
  (CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange) -> frame $ do
    iconButton_ $ defaultIconButtonProps @[GlobalAction]
      & iconButtonPropsListKey      .~ "comment-range"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") Large
      & iconButtonPropsElementName  .~ "btn-new-ann-text" -- RENAME: ann => comment
      & iconButtonPropsLabel        .~ "text-specific comment"
      & iconButtonPropsOnClick      .~ [HeaderAction StartTextSpecificComment]
      & iconButtonPropsClickPropag  .~ False
    iconButton_ $ defaultIconButtonProps @[GlobalAction]
      & iconButtonPropsListKey      .~ "comment-all"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") Large
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

instance UnoverlapAllEq EditToolbarExtensionProps

editToolbarExtension :: View '[EditToolbarExtensionProps]
editToolbarExtension = mkView "EditToolbarExtension" $ \case
  (EditToolbarExtensionProps EditToolbarExtension) -> editKindForm_ (HeaderAction . StartEdit) (EditKindFormProps Nothing)
  (EditToolbarExtensionProps _) -> mempty

editToolbarExtension_ :: EditToolbarExtensionProps -> ReactElementM handler ()
editToolbarExtension_ !props = view_ editToolbarExtension "editToolbarExtension_" props


newtype EditKindFormProps = EditKindFormProps (Maybe EditKind)
  deriving (Eq)

instance UnoverlapAllEq EditKindFormProps

-- | FIXME: this component should be moved closer to "Refine.Frontend.Contribution.Dialog".  (not
-- sure about the structure in general.  perhaps more code shuffling is indicated at some point.)
editKindForm :: (EditKind -> GlobalAction) -> View '[EditKindFormProps]
editKindForm onSelect = mkView "EditKindForm" $ \(EditKindFormProps mactive) -> do
    div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)
            editButton mactive `mapM_` [Grammar, Phrasing, Meaning]
  where
    editButton :: Maybe EditKind -> EditKind -> ReactElementM eventHandler ()
    editButton mactive kind =
      let size = if Just kind == mactive then XXLarge else Large in
      iconButton_ $ defaultIconButtonProps @[GlobalAction]
        & iconButtonPropsListKey      .~ cs (show kind)
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-New_Edit", "dark") size
        & iconButtonPropsElementName  .~ "btn-new-mod-text" -- RENAME: mod => edit
        & iconButtonPropsLabel        .~ cs (show kind)
        & iconButtonPropsOnClick      .~ [onSelect kind]
        & iconButtonPropsClickPropag  .~ False

editKindForm_ :: (EditKind -> GlobalAction) -> EditKindFormProps -> ReactElementM ViewEventHandler ()
editKindForm_ onSelect = view_ (editKindForm onSelect) "editToolbarExtension_"
