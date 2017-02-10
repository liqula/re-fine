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
import           Control.Monad (forM_)
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Lifecycle

import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


menuButton :: ReactView ()
menuButton = defineView "MenuButton" $ \() ->
  span_ ["className" $= "c-mainmenu"] $ do
    button_ ["aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

menuButton_ :: ReactElementM eventHandler ()
menuButton_ = view menuButton () mempty


headerSizeCapture :: ReactView ()
headerSizeCapture = defineLifecycleView "HeaderSizeCapture" () lifecycleConfig
   { lRender = \_state _props ->
       div_ ["className" $= "c-fullheader"] childrenPassedToView
   , lComponentDidMount = Just $ \_propsandstate ldom _ -> do
             this <- lThis ldom
             height <- js_getBoundingClientRectHeight this
             _ <- forkIO $ do
                 let actions = RS.dispatch $ RS.AddHeaderHeight height
                 forM_ actions executeAction
             pure ()
   }

headerSizeCapture_ :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
headerSizeCapture_ = view headerSizeCapture ()

foreign import javascript unsafe
  "$1.getBoundingClientRect().height"
  js_getBoundingClientRectHeight :: JSVal -> IO Int
