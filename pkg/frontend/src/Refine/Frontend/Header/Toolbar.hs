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

import           Refine.Frontend.Prelude
import           Language.Css.Syntax

import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Util

toolbar_ :: HasCallStack => ReactElementM eventHandler ()
toolbar_ = do
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
    & iconButtonPropsOnClick      .~ [HeaderAction StartEdit]
    & iconButtonPropsClickPropag  .~ False

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "all-comments"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Comment", "dark") XXLarge
    & iconButtonPropsElementName  .~ "all-annotations"   -- RENAME: annotation => comment
    & iconButtonPropsLabel        .~ "all comments"
    & iconButtonPropsOnClick      .~ (ContributionAction <$> [ SetBubbleFilter Nothing
                                                             , SetBubblePositioning BubblePositioningAbsolute
                                                             ])

  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "all-edits"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXLarge
    & iconButtonPropsElementName  .~ "all-modifications"  -- RENAME: edit
    & iconButtonPropsLabel        .~ "all edits"
    & iconButtonPropsOnClick      .~ (ContributionAction <$> [ SetBubbleFilter Nothing
                                                             , SetBubblePositioning BubblePositioningAbsolute
                                                             ])

  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "read-only"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Reader", "dark") XXLarge
    & iconButtonPropsElementName  .~ "btn-read-mode"
    & iconButtonPropsLabel        .~ "read mode"
    & iconButtonPropsOnClick      .~ [HeaderAction ToggleReadOnly]
    & iconButtonPropsAlignRight   .~ True


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: ToolbarExtensionStatus
  }
  deriving (Eq)

instance UnoverlapAllEq CommentToolbarExtensionProps

commentToolbarExtension :: HasCallStack => View '[CommentToolbarExtensionProps]
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

    div_ ["className" $= "c-vdoc-toolbar__separator"] ""

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


commentToolbarExtension_ :: HasCallStack => CommentToolbarExtensionProps -> ReactElementM eventHandler ()
commentToolbarExtension_ = view_ commentToolbarExtension "commentToolbarExtension_"


editToolbarExtension :: View '[EditToolbarExtensionProps]
editToolbarExtension = mkView "EditToolbarExtension" $ \case
  (EditToolbarExtensionProps (EditToolbarLinkEditor link)) -> editLinkInput_ link
  (EditToolbarExtensionProps _) -> mempty

editToolbarExtension_ :: EditToolbarExtensionProps -> ReactElementM handler ()
editToolbarExtension_ = view_ editToolbarExtension "editToolbarExtension_"


newtype EditToolbarExtensionProps = EditToolbarExtensionProps
  { _etepStatus :: ToolbarExtensionStatus
  }
  deriving (Eq)

instance UnoverlapAllEq EditToolbarExtensionProps


linkToolbarTextForm :: HasCallStack => ST -> ReactElementM ('StatefulEventHandlerCode AddLinkFormState) ()
linkToolbarTextForm link = do
  form_ [ "target" $= "#"
        , "action" $= "POST"
        , "style"  @@= [decl "width" (Percentage 80)]
        ] $ do
    textarea_ [ "style" @@=
                      [ decl "resize" (Ident "none")
                      , decl "width" (Percentage 100)
                      , decl "height" (Rem 1.2)
                      , decl "vertical-align" (Ident "middle")
                      ]
              , "placeholder" $= "url"
              -- Update the current state with the current text in the textbox, sending no actions
              , onChange $ \evt -> simpleHandler $ \st -> ([], Just $ st & addLinkFormState .~ target evt "value")
              ]
      $ elemText link


editLinkInput :: HasCallStack => ST -> View '[]
editLinkInput link = mkStatefulView "EditLinkInput" (AddLinkFormState link) $ \curState -> do
    div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)

            span_ ["style" @@= [decl "margin-right" (Px 20)]] $
              let props :: IbuttonProps [GlobalAction]
                  props = emptyIbuttonProps "Save" onclick
                    & ibListKey        .~ "add-link"
                    & ibSize           .~ Large
                    & ibDarkBackground .~ True
                  onclick = [ DocumentAction . DocumentCreateLink $ curState ^. addLinkFormState
                            , HeaderAction CloseToolbarExtension
                            ]
               in ibutton_ props

            span_ ["style" @@= [decl "margin-right" (Px 20)]] $
              let props :: IbuttonProps [GlobalAction]
                  props = emptyIbuttonProps "Close" onclick
                    & ibListKey        .~ "cancel"
                    & ibSize           .~ Large
                    & ibDarkBackground .~ True
                  onclick = [HeaderAction CloseToolbarExtension]
               in ibutton_ props

            span_ ["style" @@= [decl "margin-right" (Px 20), decl "width" (Percentage 100)]] $
              linkToolbarTextForm link

editLinkInput_ :: HasCallStack => ST -> ReactElementM eventHandler ()
editLinkInput_ link = view_ (editLinkInput link) "editLinkInput_"
