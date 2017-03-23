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
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets

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
            & iconButtonPropsClickHandler .~ (\_ -> RS.dispatch RS.ShowNotImplementedYet)

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-comment"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXL
            & iconButtonPropsElementName  .~ "btn-add-annotation"
            & iconButtonPropsContentType  .~ "comment"
            & iconButtonPropsLabel        .~ "new comment"
            & iconButtonPropsClickHandler .~ (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.ToggleCommentToolbarExtension))

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "new-edit"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXL
            & iconButtonPropsElementName  .~ "bt-add-modification"  -- RENAME: edit
            & iconButtonPropsContentType  .~ "edit"
            & iconButtonPropsLabel        .~ "new edit"
            & iconButtonPropsClickHandler .~ (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.ToggleEditToolbarExtension))

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-comments"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" False ("icon-Comment", "dark") XXL
            & iconButtonPropsElementName  .~ "all-annotations"   -- RENAME: annotation => comment
            & iconButtonPropsLabel        .~ "all comments"
            & iconButtonPropsClickHandler .~ (\_ -> RS.dispatch RS.ShowNotImplementedYet)

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "all-edits"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXL
            & iconButtonPropsElementName  .~ "all-modifications"  -- RENAME: edit
            & iconButtonPropsLabel        .~ "all edits"
            & iconButtonPropsClickHandler .~ (\_ -> RS.dispatch RS.ShowNotImplementedYet)

          iconButton_ $ toolbarButton
            & iconButtonPropsListKey      .~ "read-only"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Reader", "bright") XXL
            & iconButtonPropsElementName  .~ "btn-read-mode"
            & iconButtonPropsLabel        .~ "read mode"
            & iconButtonPropsClickHandler .~ (\_ -> RS.dispatch RS.ShowNotImplementedYet)
            & iconButtonPropsAlignRight   .~ True

toolbar_ :: ReactElementM eventHandler ()
toolbar_ = view_ toolbar "toolbar_" ()


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: RS.ToolbarExtensionStatus
  }

commentToolbarExtension :: View '[CommentToolbarExtensionProps]
commentToolbarExtension = mkView "CommentToolbarExtension" $ \case
  (CommentToolbarExtensionProps RS.CommentToolbarExtensionWithSelection) -> frame $ do
    div_ "Please select the text you would like to comment on"
  (CommentToolbarExtensionProps RS.CommentToolbarExtensionWithButtons) -> frame $ do
    iconButton_ $ def
      & iconButtonPropsListKey      .~ "comment-range"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L
      & iconButtonPropsElementName  .~ "btn-new-ann-text" -- RENAME: ann => comment
      & iconButtonPropsContentType  .~ "comment"
      & iconButtonPropsLabel        .~ "text-specific comment"
      & iconButtonPropsClickHandler .~ (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.StartTextSpecificComment))
    iconButton_ $ def
      & iconButtonPropsListKey      .~ "comment-all"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L
      & iconButtonPropsElementName  .~ "btn-new-ann-doc"  -- RENAME: ann => comment
      & iconButtonPropsContentType  .~ "comment"
      & iconButtonPropsLabel        .~ "general comment"
      & iconButtonPropsClickHandler .~ (\_ -> RS.dispatch RS.ShowNotImplementedYet)
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
        & iconButtonPropsContentType  .~ "edit"
        & iconButtonPropsLabel        .~ cs (show kind)
        & iconButtonPropsClickHandler .~ (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction (RS.StartEdit kind)))

editToolbarExtension_ :: EditToolbarExtensionProps -> ReactElementM eventHandler ()
editToolbarExtension_ !props = view_ editToolbarExtension "editToolbarExtension_" props
