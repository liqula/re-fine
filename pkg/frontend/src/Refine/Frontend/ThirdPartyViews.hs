{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.ThirdPartyViews
  ( ThirdPartyView
  , skylight_
  , hammer_
  , draftEditor_
  ) where
#include "import_frontend.hs"


type ThirdPartyView eventHandler = [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()

skylight_ :: HasCallStack => ThirdPartyView eventHandler
skylight_ = foreignClass js_skylight

hammer_ :: HasCallStack => ThirdPartyView eventHandler
hammer_ = foreignClass js_hammer

draftEditor_ :: HasCallStack => ThirdPartyView eventHandler
draftEditor_ = foreignClass js_draftEditor

#ifdef __GHCJS__

foreign import javascript safe
  "Skylight.SkyLightStateless"
  js_skylight :: JSVal

foreign import javascript safe
  "Hammer"
  js_hammer :: JSVal

foreign import javascript safe
  "Draft.Editor"
  js_draftEditor :: JSVal

#else

{-# ANN js_skylight ("HLint: ignore Use camelCase" :: String) #-}
js_skylight :: JSVal
js_skylight = error "javascript FFI not available in GHC"

{-# ANN js_hammer ("HLint: ignore Use camelCase" :: String) #-}
js_hammer :: JSVal
js_hammer = error "javascript FFI not available in GHC"

{-# ANN js_draftEditor ("HLint: ignore Use camelCase" :: String) #-}
js_draftEditor :: JSVal
js_draftEditor = error "javascript FFI not available in GHC"

#endif
