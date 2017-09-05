{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Route
  ( module Refine.Frontend.Route
  , module Refine.Common.Route
  ) where
#include "import_frontend.hs"

import GHCJS.Foreign.Callback (Callback, asyncCallback1)
import Refine.Common.Route


onLocationHashChange :: (Either RouteParseError Route -> IO ()) -> IO ()
onLocationHashChange f = do
  cb <- asyncCallback1 (f . rparse . pFromJSVal)
  js_attachLocationHashCb cb

currentRoute :: MonadIO m => m (Either RouteParseError Route)
currentRoute = rparse . cs <$> liftIO js_getLocationHash

changeRoute :: MonadIO m => Route -> m ()
changeRoute = liftIO . js_setLocationHash . cs . rrender


-- * foreign

#ifdef __GHCJS__

foreign import javascript unsafe
  "window.onhashchange = function() {$1(location.hash.toString());};"
  js_attachLocationHashCb :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "window.location.hash"
  js_getLocationHash :: IO JSString

foreign import javascript safe
  "window.location.hash = $1;"
  js_setLocationHash :: JSString -> IO ()

#else

{-# ANN js_attachLocationHashCb ("HLint: ignore Use camelCase" :: String) #-}
js_attachLocationHashCb :: Callback (JSVal -> IO ()) -> IO ()
js_attachLocationHashCb = error "javascript FFI not available in GHC"

{-# ANN js_getLocationHash ("HLint: ignore Use camelCase" :: String) #-}
js_getLocationHash :: IO JSString
js_getLocationHash = error "javascript FFI not available in GHC"

{-# ANN js_setLocationHash ("HLint: ignore Use camelCase" :: String) #-}
js_setLocationHash :: JSString -> IO ()
js_setLocationHash = error "javascript FFI not available in GHC"

#endif
