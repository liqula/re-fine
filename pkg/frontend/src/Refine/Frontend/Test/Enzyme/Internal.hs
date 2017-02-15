module Refine.Frontend.Test.Enzyme.Internal
 ( execWithSelector
 , execWith1Arg
 , exec
 , attr
 ) where

import GHCJS.Marshal.Pure
import GHCJS.Types (JSString, JSVal)
import React.Flux.Internal (toJSString)

import Refine.Frontend.Test.Enzyme.Core

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- Preparations for the evaluation of functions in JavaScript --------------------------------------------------

execWithSelector :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> EnzymeSelector -> IO a
execWithSelector func wrapper es@(PropertySelector _) = pFromJSVal <$> js_exec_with_object (toJSString func) (unWrap wrapper) (pToJSVal es)
execWithSelector f w e = execWith1Arg f w e

execWith1Arg :: (PFromJSVal a, PToJSVal b, EnzymeWrapper w) => String -> w -> b -> IO a
execWith1Arg func wrapper arg = pFromJSVal <$> js_exec_with_1_arg (toJSString func) (unWrap wrapper) (pToJSVal arg)

exec :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
exec func wrapper = pFromJSVal <$> js_exec (toJSString func) (unWrap wrapper)

attr :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
attr name wrapper = pFromJSVal <$> js_attr (toJSString name) (unWrap wrapper)


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
