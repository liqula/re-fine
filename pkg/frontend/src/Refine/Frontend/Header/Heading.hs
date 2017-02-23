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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.Heading where

import           Control.Lens ((^.))
import           Control.Monad (forM_, unless)
import           GHC.Generics
import           GHCJS.Types (JSVal)
import           GHCJS.Marshal.Pure
import           React.Flux
import           React.Flux.Internal (HandlerArg(HandlerArg))
import           React.Flux.Lifecycle

import           Refine.Common.Types
import           Refine.Frontend.Header.DocumentHeader ( documentHeader_, DocumentHeaderProps(..) )
import           Refine.Frontend.Header.Toolbar ( CommentToolbarExtensionProps(..), toolbar_, commentToolbarExtension_, editToolbarExtension_ )
import qualified Refine.Frontend.Header.Types as HT
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.ThirdPartyViews (sticky_)
import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.MainMenu.Types as RS
import qualified Refine.Frontend.Screen.Types as RS


newtype MenuButtonProps = MenuButtonProps Bool
  deriving (Eq, Generic)

menuButton :: ReactView MenuButtonProps
menuButton = defineView "MenuButton" $ \(MenuButtonProps sticky) ->
  span_ [classNames [("c-mainmenu", True), ("c-mainmenu--toolbar-combined", sticky)]] $ do
    button_ ["aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"
            , onClick $ \_ _ -> RS.dispatch . RS.MainMenuAction $ RS.MainMenuActionOpen RS.defaultMainMenuTab
            ] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    unless sticky $
      span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

menuButton_ :: MenuButtonProps -> ReactElementM eventHandler ()
menuButton_ props = view menuButton props mempty


-- | extract the new state from event.
currentToolbarStickyState :: Event -> Bool
currentToolbarStickyState (evtHandlerArg -> HandlerArg j) = pFromJSVal j

mainHeader :: ReactView RS.GlobalState
mainHeader = defineLifecycleView "HeaderSizeCapture" () lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { lRender = \_state rs ->
        case rs ^. RS.gsVDoc of
          Nothing -> error "mainHeader may only be invoked after a VDoc has been loaded!"
          Just vdoc ->
            div_ ["className" $= "c-fullheader"] $ do
                -- the following need to be siblings because of the z-index handling
                div_ ["className" $= "c-mainmenu__bg"] "" -- "role" $= "navigation"
                --header_ ["role" $= "banner"] $ do
                menuButton_ (MenuButtonProps $ rs ^. RS.gsToolbarSticky)
                documentHeader_ $ DocumentHeaderProps (vdoc ^. compositeVDoc . vdocTitle) (vdoc ^. compositeVDoc . vdocAbstract)
                div_ ["className" $= "c-fulltoolbar"] $ do
                    sticky_ [on "onStickyStateChange" $ \e _ -> (RS.dispatch . RS.ToolbarStickyStateChange $ currentToolbarStickyState e, Nothing)] $ do
                        toolbar_
                        commentToolbarExtension_ $ CommentToolbarExtensionProps (rs ^. RS.gsHeaderState . HT.hsToolbarExtensionStatus)
                        editToolbarExtension_

   , lComponentDidMount  = Just $ \_propsandstate ldom _     -> calcHeaderHeight ldom
   -- , lComponentDidUpdate = Just $ \_propsandstate ldom _ _ _ -> calcHeaderHeight ldom
   }

calcHeaderHeight :: LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- lThis ldom
   height <- js_getBoundingClientRectHeight this
   _ <- RS.reactFluxWorkAroundForkIO $ do
       let actions = RS.dispatch . RS.ScreenAction $ RS.AddHeaderHeight height
       forM_ actions executeAction
   pure ()

mainHeader_ :: RS.GlobalState -> ReactElementM eventHandler ()
mainHeader_ props = view mainHeader props mempty

foreign import javascript unsafe
  "$1.getBoundingClientRect().height"
  js_getBoundingClientRectHeight :: JSVal -> IO Int
