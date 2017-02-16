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

module Refine.Frontend.Screen.WindowSize where

import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           React.Flux
import           React.Flux.Lifecycle
import           GHCJS.Foreign.Callback (Callback, asyncCallback)
import           GHCJS.Types (JSString)

import qualified Refine.Frontend.Screen.Types as RS
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


newtype WindowSizeProps = WindowSizeProps
    { _currentSize :: RS.WindowSize
    }

windowSize :: ReactView WindowSizeProps
windowSize = defineLifecycleView "WindowSize" () lifecycleConfig
   { lRender = \_state (WindowSizeProps size) ->
         -- TODO debug output; remove in production
         span_ ["className" $= "layout-indicator"] . elemString $ "layout: " <> show size
   , lComponentDidMount = Just $ \_ _ _ -> do
           cb <- asyncCallback setWindowSize
           js_windowAddEventListener "resize" cb
           setWindowSize
   , lComponentWillUnmount = Just $ \_ _ -> do
           cb <- asyncCallback setWindowSize
           js_windowRemoveEventListener "resize" cb
   }

windowSize_ :: WindowSizeProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
windowSize_ = view windowSize

setWindowSize :: IO ()
setWindowSize = do
    width <- js_getWindowWidth
    _ <- RS.reactFluxWorkAroundForkIO $ do
       let actions = RS.dispatch . RS.SetWindowSize $ RS.toSize width
       forM_ actions executeAction
    pure ()

foreign import javascript unsafe
-- the internet says we should check window.innerWidth and document.documentElement.clientWidth first,
-- and we should get the body via document.getElementsByTagName('body')[0]
-- but Tom does not do that either -- so?!
-- (In my Chrome, they are all available and contain the same values anyway...)
  "document.body.clientWidth"
  js_getWindowWidth :: IO Int


foreign import javascript unsafe
    "window.addEventListener($1, $2)"
    js_windowAddEventListener :: JSString -> Callback (IO ()) -> IO ()

foreign import javascript unsafe
    "window.removeEventListener($1, $2)"
    js_windowRemoveEventListener :: JSString -> Callback (IO ()) -> IO ()
