module Refine.Frontend.Test.Enzyme.Internal where

import GHCJS.Types (JSString, JSVal)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


foreign import javascript unsafe
    "$2[$1]()"
    js_exec :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$2[$1]"
    js_attr :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_with_1_arg :: JSString -> JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$2[$1](JSON.parse($3))"
    js_exec_with_object :: JSString -> JSVal -> JSVal -> IO JSVal
