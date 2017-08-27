{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.Heading
  ( TopMenuBarProps(..)
  , topMenuBar, topMenuBar_
  , mainHeader, mainHeader_
  , toolbarWrapper_
  , mkMainHeaderProps
  , mainHeaderRender2
  ) where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           Refine.Common.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.DocumentHeader
import           Refine.Frontend.Header.DiffToolbar ( diffToolbar_ )
import           Refine.Frontend.Header.EditToolbar ( editToolbar_, wipeDocumentState )
import           Refine.Frontend.Header.DiscussionToolbar
import           Refine.Frontend.Header.Toolbar ( CommentToolbarExtensionProps(..), EditToolbarExtensionProps(..),
                                                  toolbar_, commentToolbarExtension_, editToolbarExtension_, indexToolbarExtension_ )
import           Refine.Frontend.Access
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util


topMenuBar :: HasCallStack => View '[TopMenuBarProps]
topMenuBar = mkView "TopMenuBar" $ \props ->
  div_ [ classNamesAny [("c-mainmenu", True)]
       , "style" @@= [decl "pointerEvents" (Ident "none")]
       , "id" $= "top-menubar"
       ] $ do
    topMenuBarLeft_ props
    topMenuBarRight_ props

topMenuBar_ :: HasCallStack => TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBar_ = view_ topMenuBar "TopMenuBar_"

topMenuBarLeft :: View '[TopMenuBarProps]
topMenuBarLeft = mkView "TopMenuBarLeft" $ \(TopMenuBarProps _currentUser) -> do
    button_ [ "aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"
            , "style" @@= [decl "pointerEvents" (Ident "all")]
            , onClick $ \_ _ -> simpleHandler . dispatch . MainMenuAction $ MainMenuActionOpen defaultMainMenuTab
            ] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""

    span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

topMenuBarLeft_ :: TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBarLeft_ = view_ topMenuBarLeft "TopMenuBarLeft_"

topMenuBarRight_ :: TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBarRight_ (TopMenuBarProps cu) = do
    loginStatusButton_ id cu


-- | Note that if @toolbarItems_@ is a component ('View') rather than a 'ReactElementM', css styling
-- mysteriously breaks.
toolbarWrapper_ :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
toolbarWrapper_ toolbarItems_ = do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do
          toolbarItems_

mainHeader :: HasCallStack => React.ReactView MainHeaderProps
mainHeader = React.defineLifecycleView "HeaderSizeCapture" () React.lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { React.lRender = mainHeaderRender
   , React.lComponentDidMount = Just $ \_ dom _ -> calcHeaderHeight dom
   , React.lComponentDidUpdate = Just $ \_ dom _ _ _ -> calcHeaderHeight dom
   }

mainHeaderRender :: HasCallStack => () -> MainHeaderProps -> ReactElementM eventHandler ()
mainHeaderRender () (rs, as) = do
  let vdoc = fromMaybe (error "mainHeader: no vdoc!") $ rs ^? gsVDoc . _Just . _Just
      props = TopMenuBarProps (as ^. accLoginState . lsCurrentUser)

      mainMenuPart_ = do
        -- in the past, the following needed to be siblings because of the z-index handling.  not sure that's still the case.
        div_ ["className" $= "c-mainmenu__bg" {-, "role" $= "navigation" -}] mempty
        {- header_ ["role" $= "banner"] $ do -}
        topMenuBar_ props

      headerPart_
        = documentHeader_
        . DocumentHeaderProps (vdoc ^. compositeVDoc . vdocTitle)
        $ case rs ^. gsDocumentState of
          WipedDocumentStateDiff _ eid _ _ -> editDescToAbstract vdoc . ContribIDEdit $ eid ^. editID
          _ -> vdoc ^. compositeVDoc . vdocAbstract

  div_ ["className" $= "c-fullheader"] $ do
      mainMenuPart_
      headerPart_

mainHeaderRender2 :: HasCallStack => MainHeaderProps -> ReactElementM eventHandler ()
mainHeaderRender2 (rs, _as) = div_ ["className" $= "c-fulltoolbar"] $ do
          toolbarWrapper_ $ case rs ^. gsDocumentState of

            WipedDocumentStateView -> toolbar_ . fromJust $ rs ^? gsVDoc . _Just . _Just . compositeVDoc

            WipedDocumentStateDiff i edit collapsed editable -> diffToolbar_ $ DiffToolbarProps
              (edit ^. editID)
              i
              (edit ^. editKind)
              (edit ^. editVotes . to votesToCount)
              collapsed
              editable

            WipedDocumentStateEdit eprops -> editToolbar_ eprops

            WipedDocumentStateDiscussion dprops -> discussionToolbar_ dprops

          indexToolbarExtension_ $ mkIndexToolbarProps rs
          commentToolbarExtension_ $ CommentToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)
          editToolbarExtension_ $ EditToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)

mkIndexToolbarProps :: GlobalState_ WipedDocumentState -> IndexToolbarProps
mkIndexToolbarProps rs
  | rs ^. gsHeaderState . hsToolbarExtensionStatus == IndexToolbarExtension
  && fromMaybe True (not <$> rs ^? gsDocumentState . wipedDocumentStateDiffCollapsed)
  = mkIndex . _editVDocVersion <$> join (gsEdit rs)
  | otherwise = Nothing
  where
    mkIndex (RawContent bs _) =
      [ IndexItem (b ^. blockKey) (b ^. blockText) depth
      | b <- NEL.toList bs, depth <- maybeToList . headerDepth $ b ^. blockType
      ]

headerDepth :: BlockType -> Maybe Int
headerDepth = \case
  Header1 -> Just 1
  Header2 -> Just 2
  Header3 -> Just 3
  _ -> Nothing

mkMainHeaderProps :: AccessState -> GlobalState -> MainHeaderProps
mkMainHeaderProps as gs = (fmap (const $ wipeDocumentState as gs) gs, as)

mainHeader_ :: HasCallStack => MainHeaderProps -> ReactElementM eventHandler ()
mainHeader_ props = React.viewWithSKey mainHeader "mainHeader" props mempty

calcHeaderHeight :: HasCallStack => React.LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- React.lThis ldom
   h <- js_getHeaderHeight this
   when (h /= (-1)) . dispatchAndExec . ScreenAction . AddHeaderHeight $ h + 80

#ifdef __GHCJS__

foreign import javascript safe
  "refine$getHeaderHeight($1)"
  js_getHeaderHeight :: JSVal -> IO Int

#else

{-# ANN js_getHeaderHeight ("HLint: ignore Use camelCase" :: String) #-}
js_getHeaderHeight :: JSVal -> IO Int
js_getHeaderHeight = error "javascript FFI not available in GHC"

#endif
