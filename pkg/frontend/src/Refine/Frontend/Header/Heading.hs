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

import           Control.Concurrent (forkIO)
import           Control.Monad (forM_, unless)
import           GHC.Generics
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Lifecycle

import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


newtype MenuButtonProps = MenuButtonProps Bool
  deriving (Eq, Generic)

menuButton :: ReactView MenuButtonProps
menuButton = defineView "MenuButton" $ \(MenuButtonProps sticky) ->
  span_ [classNames [("c-mainmenu", True), ("c-mainmenu--toolbar-combined", sticky)]] $ do
    button_ ["aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"
            , onClick $ \_ _ -> RS.dispatch RS.ToggleMainMenu
            ] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    unless sticky $
      span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

menuButton_ :: MenuButtonProps -> ReactElementM eventHandler ()
menuButton_ props = view menuButton props mempty


headerSizeCapture :: ReactView ()
headerSizeCapture = defineLifecycleView "HeaderSizeCapture" () lifecycleConfig
     -- the render function inside a Lifecycle view does not update its children when the state changes
     -- (see react-flux issue #29), therefore we don't render anything inside a Lifecylce view.
   { lRender = \_state _props -> mempty
   , lComponentDidMount = Just $ \_propsandstate ldom _ -> do
             this <- lThis ldom
             height <- js_getBoundingClientRectHeight this
             _ <- forkIO $ do
                 let actions = RS.dispatch $ RS.AddHeaderHeight height
                 forM_ actions executeAction
             pure ()
   }

headerSizeCapture_ :: ReactElementM eventHandler ()
headerSizeCapture_ = view headerSizeCapture () mempty

foreign import javascript unsafe
  "$1.getBoundingClientRect().height"
  js_getBoundingClientRectHeight :: JSVal -> IO Int
