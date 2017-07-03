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

module Refine.Frontend.Screen.WindowSize (WindowSizeProps(..), windowSize_) where

import Refine.Frontend.Prelude

import           React.Flux.Outdated as RF
import           GHCJS.Foreign.Callback (Callback, asyncCallback)

import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console (weAreInDevMode)


newtype WindowSizeProps = WindowSizeProps
  { _currentSize :: WindowSize
  }
  deriving (Eq)

windowSize :: HasCallStack => ReactView WindowSizeProps
windowSize = defineLifecycleView "WindowSize" () lifecycleConfig
   { lRender = \_state (WindowSizeProps size) ->
       if weAreInDevMode
         then span_ ["className" $= "layout-indicator"] . elemString $ "layout: " <> show size
         else pure ()
   , lComponentDidMount = Just $ \_ _ _ -> didMountOrUpdate
   , lComponentDidUpdate = Just $ \_ _ _ _ _ -> didMountOrUpdate
   , lComponentWillUnmount = Just $ \_ _ -> willUnmount
   }

didMountOrUpdate :: IO ()
didMountOrUpdate = do
  cb <- asyncCallback setWindowSize
  js_windowAddEventListener "resize" cb
  setWindowSize

willUnmount :: IO ()
willUnmount = do
  cb <- asyncCallback setWindowSize
  js_windowRemoveEventListener "resize" cb

windowSize_ :: HasCallStack => WindowSizeProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
windowSize_ = RF.view windowSize

setWindowSize :: HasCallStack => IO ()
setWindowSize = dispatchAndExec . ScreenAction . SetWindowWidth =<< js_getWindowWidth

#ifdef __GHCJS__

-- (in case this didn't work: the internet says we should check window.innerWidth and
-- document.documentElement.clientWidth first, and we should get the body via
-- document.getElementsByTagName('body')[0])
foreign import javascript safe
  "document.body.clientWidth"
  js_getWindowWidth :: IO Int

foreign import javascript safe
    "window.addEventListener($1, $2)"
    js_windowAddEventListener :: JSString -> Callback (IO ()) -> IO ()

foreign import javascript safe
    "window.removeEventListener($1, $2)"
    js_windowRemoveEventListener :: JSString -> Callback (IO ()) -> IO ()

#else

{-# ANN js_getWindowWidth ("HLint: ignore Use camelCase" :: String) #-}
js_getWindowWidth :: IO Int
js_getWindowWidth = error "javascript FFI not available in GHC"

{-# ANN js_windowAddEventListener ("HLint: ignore Use camelCase" :: String) #-}
js_windowAddEventListener :: JSString -> Callback (IO ()) -> IO ()
js_windowAddEventListener = error "javascript FFI not available in GHC"

{-# ANN js_windowRemoveEventListener ("HLint: ignore Use camelCase" :: String) #-}
js_windowRemoveEventListener :: JSString -> Callback (IO ()) -> IO ()
js_windowRemoveEventListener = error "javascript FFI not available in GHC"

#endif
