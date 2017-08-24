{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.ThirdPartyViews
  ( ThirdPartyView
  , skylight_
  , hammer_
  , editor_
  ) where

import Refine.Frontend.Prelude


type ThirdPartyView eventHandler = [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()

skylight_ :: HasCallStack => ThirdPartyView eventHandler
skylight_ = foreignClass js_skylight

hammer_ :: HasCallStack => ThirdPartyView eventHandler
hammer_ = foreignClass js_hammer

editor_ :: HasCallStack => ThirdPartyView eventHandler
editor_ = foreignClass js_editor

#ifdef __GHCJS__

foreign import javascript safe
  "Skylight.SkyLightStateless"
  js_skylight :: JSVal

foreign import javascript safe
  "Hammer"
  js_hammer :: JSVal

foreign import javascript safe
  "Draft.Editor"
  js_editor :: JSVal

#else

{-# ANN js_skylight ("HLint: ignore Use camelCase" :: String) #-}
js_skylight :: JSVal
js_skylight = error "javascript FFI not available in GHC"

{-# ANN js_hammer ("HLint: ignore Use camelCase" :: String) #-}
js_hammer :: JSVal
js_hammer = error "javascript FFI not available in GHC"

{-# ANN js_editor ("HLint: ignore Use camelCase" :: String) #-}
js_editor :: JSVal
js_editor = error "javascript FFI not available in GHC"

#endif
