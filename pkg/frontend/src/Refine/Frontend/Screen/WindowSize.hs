{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Screen.WindowSize (trackWindowSize) where
#include "import_frontend.hs"

import           Control.Concurrent.MVar
import           GHCJS.Foreign.Callback (Callback, asyncCallback)
import           React.Flux.Outdated
import           System.IO.Unsafe (unsafePerformIO)

import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store ()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console (weAreInDevMode)


trackWindowSize :: HasCallStack => ReactView WindowSize
trackWindowSize = defineLifecycleView "TrackWindowSize" () lifecycleConfig
   { lRender = \_state _size -> do
       -- when weAreInDevMode $ do
       --   span_ ["className" $= "layout-indicator"] . elemString $ "layout: " <> show size
       pure ()
   , lComponentDidMount = Just $ \_ _ _ -> didMountOrUpdate
   , lComponentDidUpdate = Just $ \_ _ _ _ _ -> didMountOrUpdate
   , lComponentWillUnmount = Just $ \_ _ -> willUnmount
   }

{-# NOINLINE trackWindowSizeCb #-}
trackWindowSizeCb :: MVar (Callback (IO ()))
trackWindowSizeCb = unsafePerformIO $ newMVar =<< asyncCallback setWindowSize

didMountOrUpdate :: IO ()
didMountOrUpdate = do
  cb <- readMVar trackWindowSizeCb
  js_windowAddEventListener "resize" cb
  setWindowSize

willUnmount :: IO ()
willUnmount = do
  cb <- readMVar trackWindowSizeCb
  js_windowRemoveEventListener "resize" cb

setWindowSize :: HasCallStack => IO ()
setWindowSize = do
  w <- js_getWindowWidth
  when (w /= (-1)) . dispatchAndExec . ScreenAction $ SetWindowWidth w

#ifdef __GHCJS__

-- (in case this didn't work: the internet says we should check window.innerWidth and
-- document.documentElement.clientWidth first, and we should get the body via
-- document.getElementsByTagName('body')[0])
foreign import javascript safe
  "refine$documentBodyClientWidth()"
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
