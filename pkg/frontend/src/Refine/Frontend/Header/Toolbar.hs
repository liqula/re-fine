{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.Toolbar where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
-- import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import qualified Refine.Frontend.Icon.Svg as Svg
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

  let props icon acts = emptyIbuttonProps icon acts & ibSize .~ XXLarge

  ibutton_ $ props (ButtonImageIcon Svg.IndexDesktop ColorSchemaDark) [HeaderAction ToggleIndexToolbarExtension]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  -- FIXME: #358
  -- guardAccess_ "new-comment" (AP.createComments vdoc) . ...
  ibutton_ $ props (ButtonImageIcon Svg.CommentNew ColorSchemaDark)
    [LoginGuardStash [HeaderAction ToggleCommentToolbarExtension]]
  ibutton_ $ props (ButtonImageIcon Svg.EditNew ColorSchemaDark)
    [LoginGuardStash [HeaderAction StartEdit]]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  ibutton_ $ props (ButtonImageIcon Svg.Comment ColorSchemaDark)
    (ContributionAction <$> [SetBubbleFilter Nothing, SetBubblePositioning BubblePositioningAbsolute])

  ibutton_ $ props (ButtonImageIcon Svg.EditView ColorSchemaDark)
    (ContributionAction <$> [SetBubbleFilter Nothing, SetBubblePositioning BubblePositioningAbsolute])

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  -- FIXME: show this button only to process creator.
  ibutton_ $ props (ButtonImageIcon Svg.ProcessUpdate ColorSchemaDark)
    [MainMenuAction . MainMenuActionOpen . MainMenuUpdateProcess (vdoc ^. vdocID) . FormBegin $
      newLocalStateRef (UpdateVDoc (vdoc ^. vdocTitle) (vdoc ^. vdocAbstract)) vdoc]

  -- FIXME: align right.
  ibutton_ $ props (ButtonImageIcon Svg.Reader ColorSchemaDark)
    [HeaderAction ToggleReadOnly]


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: ToolbarExtensionStatus
  }
  deriving (Eq)

commentToolbarExtension :: HasCallStack => View '[CommentToolbarExtensionProps]
commentToolbarExtension = mkView "CommentToolbarExtension" $ \case
  (CommentToolbarExtensionProps CommentToolbarExtensionWithRange) -> frame $ do
    div_ "Please select the text you would like to comment on"

  (CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange) -> frame $ do
    ibutton_ $ emptyIbuttonProps
      (ButtonImageIcon Svg.Comment ColorSchemaDark)
      [HeaderAction StartTextSpecificComment]
      & ibSize .~ XLarge

    div_ ["className" $= "c-vdoc-toolbar__separator"] ""

    ibutton_ $ emptyIbuttonProps
      (ButtonImageIcon Svg.Comment ColorSchemaDark)
      [ContributionAction ShowCommentEditor]
      & ibSize .~ XLarge

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
-- FIXME: #452
--  (EditToolbarExtensionProps (EditToolbarLinkEditor link)) -> editLinkInput_ link
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


{- FIXME: #452
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
-}


indexToolbarExtension :: View '[IndexToolbarProps]
indexToolbarExtension = mkView "IndexToolbarExtension" $ \case
  Nothing -> mempty
  Just is -> frame . forM_ (zip [0 :: Int ..] is) $ \(i, item) -> do
    ul_ $ do
      li_ . a_ [ "key" $= cs (show i)
               , onClick $ \_ _ -> simpleHandler . dispatch . HeaderAction . ScrollToBlockKey $ item ^. indexItemBlockKey
               ]
        $ elemText (item ^. indexItemTitle)
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
