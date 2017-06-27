{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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

module Refine.Frontend.Header.Heading
  ( TopMenuBarProps(..)
  , topMenuBar, topMenuBar_
  , mainHeader, mainHeader_
  ) where

import Refine.Frontend.Prelude

import           Language.Css.Syntax
import qualified React.Flux as RF
import qualified React.Flux.Internal as RF
import qualified React.Flux.Outdated as RF

import           Refine.Common.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.DocumentHeader
import           Refine.Frontend.Header.DiffToolbar ( diffToolbar_ )
import           Refine.Frontend.Header.EditToolbar ( editToolbar_, mkEditToolbarProps )
import           Refine.Frontend.Header.Toolbar ( CommentToolbarExtensionProps(..), EditToolbarExtensionProps(..),
                                                  toolbar_, commentToolbarExtension_, editToolbarExtension_ )
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (sticky_)
import           Refine.Frontend.Util


topMenuBar :: HasCallStack => View '[TopMenuBarProps]
topMenuBar = mkView "TopMenuBar" $ \(TopMenuBarProps sticky currentUser) ->
  div_ [ classNamesAny [("c-mainmenu", True), ("c-mainmenu--toolbar-combined", sticky)]
       , "style" @@= [decl "pointerEvents" (Ident "none")]
       ] $ do
    topMenuBarLeft_ sticky
    topMenuBarRight_ sticky currentUser

topMenuBarLeft_ :: Bool -> ReactElementM ViewEventHandler ()
topMenuBarLeft_ sticky = do
    button_ [ "aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"
            , "style" @@= [decl "pointerEvents" (Ident "all")]
            , onClick $ \_ _ -> dispatch . MainMenuAction $ MainMenuActionOpen defaultMainMenuTab
            ] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    unless sticky $
      span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

topMenuBarRight_ :: Bool -> CurrentUser -> ReactElementM eventHandler ()
topMenuBarRight_ sticky currentUser = do
    loginStatusButton_ (ibDarkBackground .~ not sticky) currentUser

topMenuBar_ :: HasCallStack => TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBar_ = view_ topMenuBar "TopMenuBar_"


-- | extract the new state from event.
currentToolbarStickyState :: HasCallStack => Event -> Bool
currentToolbarStickyState (evtHandlerArg -> RF.HandlerArg j) = pFromJSVal j

mainHeader :: HasCallStack => RF.ReactView GlobalState
mainHeader = RF.defineLifecycleView "HeaderSizeCapture" () RF.lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { RF.lRender = mainHeaderRender
   , RF.lComponentDidMount = Just mainHeaderlComponentDidMount
   }

mainHeaderlComponentDidMount :: HasCallStack => a -> RF.LDOM -> b -> IO ()
mainHeaderlComponentDidMount _propsandstate ldom _ = calcHeaderHeight ldom

mainHeaderRender :: HasCallStack => () -> GlobalState -> ReactElementM (StatefulViewEventHandler a) ()
mainHeaderRender () rs = do
  let vdoc = fromMaybe (error "mainHeader: no vdoc!") $ rs ^? gsVDoc . _Just
      mainMenuPart_ = do
        -- in the past, the following needed to be siblings because of the z-index handling.  not sure that's still the case.
        div_ ["className" $= "c-mainmenu__bg" {-, "role" $= "navigation" -}] mempty
        {- header_ ["role" $= "banner"] $ do -}
        topMenuBar_ (TopMenuBarProps (rs ^. gsToolbarSticky) (rs ^. gsLoginState . lsCurrentUser))

      headerPart_ = documentHeader_ $ do
        let doc = DocumentHeaderProps
              (vdoc ^. compositeVDoc . vdocTitle)
              (vdoc ^. compositeVDoc . vdocAbstract)

            edit eid = DocumentHeaderProps
              (vdoc ^. compositeVDoc . vdocTitle)
              (editDescToAbstract vdoc (ContribIDEdit eid))

        case rs ^. gsDocumentState of
            DocumentStateView {}        -> doc
            DocumentStateDiff _ _ eid _ -> edit (eid ^. editID)
            DocumentStateEdit {}        -> doc

      toolbarPart_ = div_ ["className" $= "c-fulltoolbar"] $ do
        sticky_ [RF.on "onStickyStateChange" $ \e _ -> (dispatch . ToolbarStickyStateChange $ currentToolbarStickyState e, Nothing)] $ do
          case rs ^. gsDocumentState of
            DocumentStateView {} -> toolbar_
            DocumentStateDiff _ _ edit _ -> diffToolbar_ $ DiffToolbarProps
              (edit ^. editID)
              (edit ^. editVotes . to votesToCount)
            DocumentStateEdit {} -> editToolbar_ (mkEditToolbarProps rs)
          commentToolbarExtension_ $ CommentToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)
          editToolbarExtension_ $ EditToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)

  div_ ["className" $= "c-fullheader"] $ do
      mainMenuPart_
      headerPart_
      toolbarPart_

mainHeader_ :: HasCallStack => GlobalState -> ReactElementM eventHandler ()
mainHeader_ props = RF.view mainHeader props mempty

calcHeaderHeight :: HasCallStack => RF.LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- RF.lThis ldom
   dispatchAndExec . ScreenAction . AddHeaderHeight =<< js_getBoundingClientRectHeight this

#ifdef __GHCJS__

foreign import javascript safe
  "Math.floor($1.getBoundingClientRect().height)"
  js_getBoundingClientRectHeight :: JSVal -> IO Int

#else

{-# ANN js_getBoundingClientRectHeight ("HLint: ignore Use camelCase" :: String) #-}
js_getBoundingClientRectHeight :: JSVal -> IO Int
js_getBoundingClientRectHeight = error "javascript FFI not available in GHC"

#endif
