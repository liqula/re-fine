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

import qualified React.Flux as RF
import qualified React.Flux.Internal as RF
import qualified React.Flux.Outdated as RF

import           Refine.Common.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.DocumentHeader ( documentHeader_, DocumentHeaderProps(..) )
import           Refine.Frontend.Header.EditToolbar ( editToolbar_ )
import           Refine.Frontend.Header.Toolbar ( CommentToolbarExtensionProps(..), EditToolbarExtensionProps(..),
                                                  toolbar_, commentToolbarExtension_, editToolbarExtension_ )
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Header.UserLoginLogout (userLoginLogoutButton_)
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (sticky_)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types


data TopMenuBarProps = TopMenuBarProps
 { _isSticky    :: Bool
 , _currentUser :: CurrentUser
 } deriving (Eq, Generic)

instance UnoverlapAllEq TopMenuBarProps

topMenuBar :: View '[TopMenuBarProps]
topMenuBar = mkView "TopMenuBar" $ \(TopMenuBarProps sticky currentUser) ->
  span_ [classNamesAny [("c-mainmenu", True), ("c-mainmenu--toolbar-combined", sticky)]] $ do
    button_ ["aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"
            , onClick $ \_ _ -> dispatch . MainMenuAction $ MainMenuActionOpen defaultMainMenuTab
            ] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    unless sticky $
      span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"
    userLoginLogoutButton_ currentUser

topMenuBar_ :: TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBar_ !props = view_ topMenuBar "TopMenuBar_" props


-- | extract the new state from event.
currentToolbarStickyState :: Event -> Bool
currentToolbarStickyState (evtHandlerArg -> RF.HandlerArg j) = pFromJSVal j

mainHeader :: RF.ReactView GlobalState
mainHeader = RF.defineLifecycleView "HeaderSizeCapture" () RF.lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { RF.lRender = \_state rs ->
        case rs ^. gsVDoc of
          Nothing -> error "mainHeader may only be invoked after a VDoc has been loaded!"
          Just vdoc ->
            div_ ["className" $= "c-fullheader"] $ do
                -- the following need to be siblings because of the z-index handling
                div_ ["className" $= "c-mainmenu__bg"] "" -- "role" $= "navigation"
                --header_ ["role" $= "banner"] $ do
                topMenuBar_ (TopMenuBarProps (rs ^. gsToolbarSticky) (rs ^. gsLoginState . lsCurrentUser))
                documentHeader_ $ DocumentHeaderProps (vdoc ^. compositeVDoc . vdocTitle) (vdoc ^. compositeVDoc . vdocAbstract)
                div_ ["className" $= "c-fulltoolbar"] $ do
                    sticky_ [RF.on "onStickyStateChange" $ \e _ -> (dispatch . ToolbarStickyStateChange $ currentToolbarStickyState e, Nothing)] $ do
                        if has _DocumentStateView $ rs ^. gsDocumentState
                          then toolbar_
                          else editToolbar_
                        commentToolbarExtension_ $ CommentToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)
                        editToolbarExtension_ $ EditToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)

   , RF.lComponentDidMount = Just $ \_propsandstate ldom _ -> calcHeaderHeight ldom
   }

mainHeader_ :: GlobalState -> ReactElementM eventHandler ()
mainHeader_ props = RF.view mainHeader props mempty

calcHeaderHeight :: RF.LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- RF.lThis ldom
   dispatchAndExec . ScreenAction . AddHeaderHeight =<< js_getBoundingClientRectHeight this

#ifdef __GHCJS__

foreign import javascript unsafe
  "Math.floor($1.getBoundingClientRect().height)"
  js_getBoundingClientRectHeight :: JSVal -> IO Int

#else

js_getBoundingClientRectHeight :: JSVal
js_getBoundingClientRectHeight = assert False undefined

#endif
