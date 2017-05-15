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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.Heading
  ( TopMenuBarProps(..)
  , topMenuBar, topMenuBar_
  , mainHeader, mainHeader_
  ) where

import Refine.Frontend.Prelude

import           Control.Lens ((^.), has)
import           Control.Monad (unless)
import           GHC.Generics
import           GHCJS.Types (JSVal)
import           GHCJS.Marshal.Pure
import           React.Flux
import           React.Flux.Internal (HandlerArg(HandlerArg))
import           React.Flux.Outdated (ReactView, LifecycleViewConfig(..), LDOM(..), lifecycleConfig, defineLifecycleView, view)

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
currentToolbarStickyState (evtHandlerArg -> HandlerArg j) = pFromJSVal j

mainHeader :: ReactView GlobalState
mainHeader = defineLifecycleView "HeaderSizeCapture" () lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { lRender = \_state rs ->
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
                    sticky_ [on "onStickyStateChange" $ \e _ -> (dispatch . ToolbarStickyStateChange $ currentToolbarStickyState e, Nothing)] $ do
                        if has _DocumentStateView $ rs ^. gsDocumentState
                          then toolbar_
                          else editToolbar_
                        commentToolbarExtension_ $ CommentToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)
                        editToolbarExtension_ $ EditToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)

   , lComponentDidMount = Just $ \_propsandstate ldom _ -> calcHeaderHeight ldom
   }

mainHeader_ :: GlobalState -> ReactElementM eventHandler ()
mainHeader_ props = view mainHeader props mempty

calcHeaderHeight :: LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- lThis ldom
   dispatchAndExec . ScreenAction . AddHeaderHeight =<< js_getBoundingClientRectHeight this

foreign import javascript unsafe
  "Math.floor($1.getBoundingClientRect().height)"
  js_getBoundingClientRectHeight :: JSVal -> IO Int
