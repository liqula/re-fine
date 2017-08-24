{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.Toolbar where

import           Refine.Frontend.Prelude
import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
-- import qualified Refine.Common.Access.Policy as AP
-- import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Access


-- FUTUREWORK: this should probably be a component, but if we do the obvious minimal change to
-- introduce a @View '[]@, the styling breaks completely.  note that this does not fix #376 either.
toolbar_ :: HasCallStack => ToolbarProps -> ReactElementM eventHandler ()
toolbar_ vdoc = do

  -- FIXME (scss): floate this to the left
  span_ ["id" $= "c-toolbar-menu-label"
        , "className" $= "c-toolbar-menu-label-hidden"
        ] "MENU"

  let toolbarButton = defaultIconButtonProps @[GlobalAction]

  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "index"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Index_desktop", "dark") XXLarge
    & iconButtonPropsElementName  .~ "btn-index"
    & iconButtonPropsLabel        .~ "index"
    & iconButtonPropsOnClick      .~ [HeaderAction ToggleIndexToolbarExtension]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  -- FIXME: #358
  -- guardAccess_ "new-comment" (AP.createComments vdoc) . ...
  iconButton_ $ defaultIconButtonProps @[AccessAction]
    & iconButtonPropsListKey      .~ "new-comment"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXLarge
    & iconButtonPropsElementName  .~ "btn-add-annotation"  -- RENAME
    & iconButtonPropsLabel        .~ "new comment"
    & iconButtonPropsOnClick      .~ [LoginGuardStash [HeaderAction ToggleCommentToolbarExtension]]
    & iconButtonPropsOnClickMods  .~ [StopPropagation]

  iconButton_ $ defaultIconButtonProps @[AccessAction]
    & iconButtonPropsListKey      .~ "new-edit"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXLarge
    & iconButtonPropsElementName  .~ "bt-add-modification"  -- RENAME: edit
    & iconButtonPropsLabel        .~ "new edit"
    & iconButtonPropsOnClick      .~ [LoginGuardStash [HeaderAction StartEdit]]
    & iconButtonPropsOnClickMods  .~ [StopPropagation]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "all-comments"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Comment", "dark") XXLarge
    & iconButtonPropsElementName  .~ "all-annotations"   -- RENAME: annotation => comment
    & iconButtonPropsLabel        .~ "all comments"
    & iconButtonPropsOnClickMods  .~ [StopPropagation]
    & iconButtonPropsOnClick      .~ (ContributionAction <$> [ SetBubbleFilter Nothing
                                                             , SetBubblePositioning BubblePositioningAbsolute
                                                             ])

  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "all-edits"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXLarge
    & iconButtonPropsElementName  .~ "all-modifications"  -- RENAME: edit
    & iconButtonPropsLabel        .~ "all edits"
    & iconButtonPropsOnClickMods  .~ [StopPropagation]
    & iconButtonPropsOnClick      .~ (ContributionAction <$> [ SetBubbleFilter Nothing
                                                             , SetBubblePositioning BubblePositioningAbsolute
                                                             ])

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ toolbarButton  -- FIXME: show this button only to process creator.
    & iconButtonPropsListKey      .~ "update-process"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Process_update", "dark") XXLarge
    & iconButtonPropsLabel        .~ "update process"
    & iconButtonPropsOnClickMods  .~ [StopPropagation]
    & iconButtonPropsOnClick      .~ [ MainMenuAction . MainMenuActionOpen . MainMenuUpdateProcess (vdoc ^. vdocID) . FormBegin $
                                       newLocalStateRef (UpdateVDoc (vdoc ^. vdocTitle) (vdoc ^. vdocAbstract)) vdoc
                                     ]


  iconButton_ $ toolbarButton
    & iconButtonPropsListKey      .~ "read-only"
    & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Reader", "dark") XXLarge
    & iconButtonPropsElementName  .~ "btn-read-mode"
    & iconButtonPropsLabel        .~ "read mode"
    & iconButtonPropsOnClick      .~ [HeaderAction ToggleReadOnly]
    & iconButtonPropsOnClickMods  .~ [StopPropagation]
    & iconButtonPropsAlignRight   .~ True


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: ToolbarExtensionStatus
  }
  deriving (Eq)

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
      & iconButtonPropsOnClickMods  .~ [StopPropagation]

    div_ ["className" $= "c-vdoc-toolbar__separator"] ""

    iconButton_ $ defaultIconButtonProps @[GlobalAction]
      & iconButtonPropsListKey      .~ "comment-all"
      & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") Large
      & iconButtonPropsElementName  .~ "btn-new-ann-doc"  -- RENAME: ann => comment
      & iconButtonPropsLabel        .~ "general comment"
      & iconButtonPropsOnClick      .~ [ContributionAction ShowCommentEditor]
      & iconButtonPropsOnClickMods  .~ [StopPropagation]

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


indexToolbarExtension :: View '[IndexToolbarProps]
indexToolbarExtension = mkView "IndexToolbarExtension" $ \case
  Nothing -> mempty
  Just is -> frame . forM_ (zip [0 :: Int ..] is) $ \(i, item) -> do
    iconButton_
      $ defaultIconButtonProps @[GlobalAction]
      & iconButtonPropsListKey      .~ ("index-" <> cs (show i))
      & iconButtonPropsElementName  .~ ("index-heading-" <> cs (show i))
      & iconButtonPropsLabel        .~ cs (item ^. indexItemTitle)
      & iconButtonPropsOnClick      .~ [HeaderAction . ScrollToBlockKey $ item ^. indexItemBlockKey]
      & iconButtonPropsOnClickMods  .~ [StopPropagation]
    br_ []
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

indexToolbarExtension_ :: HasCallStack => IndexToolbarProps -> ReactElementM eventHandler ()
indexToolbarExtension_ = view_ indexToolbarExtension "indexToolbarExtension_"
