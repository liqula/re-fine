{-# LANGUAGE CPP                        #-}

module Refine.Frontend.Test.Enzyme.Class.Internal where

import Data.JSString (pack)
import GHCJS.Marshal.Pure
import GHCJS.Types (JSString, JSVal)

import Refine.Frontend.Test.Enzyme.Core


-- | This type class allows to define different wrappers
class PFromJSVal a => EnzymeWrapper a where
  unWrap :: a -> JSVal

  consoleLogWrapper :: JSString -> a -> IO ()
  consoleLogWrapper msg = js_console_log_jsval msg . unWrap


-- * The Enzyme API that is available for all wrappers

find :: EnzymeWrapper w => w -> EnzymeSelector -> IO w
find = execWithSelector "find"

-- FIXME: findWhere

-- FIXME: filter

-- FIXME: filterWhere

-- FIXME: contains

-- FIXME: containsMatchingElement

-- FIXME: containsAllMatchingElements

-- FIXME: containsAnyMatchingElements

-- FIXME: hasClass

is :: EnzymeWrapper w => w -> EnzymeSelector -> IO Bool
is = execWithSelector "is"

-- FIXME: exists

-- FIXME: isEmpty

-- FIXME: not

children :: EnzymeWrapper w => w -> IO w
children = exec "children"

childAt :: EnzymeWrapper w => w -> Int -> IO w
childAt = execWith1Arg "childAt"

-- FIXME: parents

-- FIXME: parent

-- FIXME: closest

-- FIXME: render
-- FIXME: CheerioWrapper

text :: EnzymeWrapper w => w -> IO JSString
text = exec "text"

html :: EnzymeWrapper w => w -> IO JSString
html = exec "html"

-- FIXME: get

-- FIXME: getNode

-- FIXME: getNodes

at :: EnzymeWrapper w => w -> Int -> IO w
at = execWith1Arg "at"

-- FIXME: first

-- FIXME: last

-- FIXME: state

-- FIXME: context

props :: EnzymeWrapper w => w -> IO JSVal
props = exec "props"

-- FIXME: prop

-- FIXME: key

simulate :: EnzymeWrapper w => w -> EventType -> IO ()
simulate = execWith1Arg "simulate"

-- FIXME: setState

-- FIXME: setProps

-- FIXME: setContext

-- FIXME: instance

-- FIXME: unmount

-- FIXME: update

-- FIXME: debug

-- | "type" in AirBnB Enzyme.
typeOf :: EnzymeWrapper w => w -> IO JSVal
typeOf = exec "type"

-- FIXME: name

-- FIXME: forEach

-- FIXME: map

-- FIXME: reduce

-- FIXME: reduceRight

-- FIXME: slice

-- FIXME: tap

-- FIXME: some

-- FIXME: someWhere

-- FIXME: every

-- FIXME: everyWhere


-- * Helper Functions

lengthOf :: EnzymeWrapper w => w -> IO Int
lengthOf = attr "length"

lengthOfIO :: EnzymeWrapper w => IO w -> IO Int
lengthOfIO wrapper = lengthOf =<< wrapper


-- * Preparations for the evaluation of functions in JavaScript

execWithSelector :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> EnzymeSelector -> IO a
execWithSelector func wrapper es@(PropertySelector _) = pFromJSVal <$> js_exec_with_object (pack func) (unWrap wrapper) (pToJSVal es)
execWithSelector func wrapper es@(StyleSelector _) = pFromJSVal <$> js_exec_with_object (pack func) (unWrap wrapper) (pToJSVal es)
execWithSelector f w e = execWith1Arg f w e

execWith1Arg :: (PFromJSVal a, PToJSVal b, EnzymeWrapper w) => String -> w -> b -> IO a
execWith1Arg func wrapper arg = pFromJSVal <$> js_exec_with_1_arg (pack func) (unWrap wrapper) (pToJSVal arg)

exec :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
exec func wrapper = pFromJSVal <$> js_exec (pack func) (unWrap wrapper)

attr :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
attr name wrapper = pFromJSVal <$> js_attr (pack name) (unWrap wrapper)


-- * The actual JavaScript calls

#ifdef __GHCJS__

foreign import javascript safe
  "$2[$1]()"
  js_exec :: JSString -> JSVal -> IO JSVal

foreign import javascript safe
  "$2[$1]"
  js_attr :: JSString -> JSVal -> IO JSVal

foreign import javascript safe
  "$2[$1]($3)"
  js_exec_with_1_arg :: JSString -> JSVal -> JSVal -> IO JSVal

-- | Log objects that have no 'PToJSVal' instance, but a 'ToJSON' instance, and thus can be
-- conveniently turned into a JSString.  (This is neither efficient nor pretty, but arguably it's
-- good enough to write useful tests.)
--
-- FIXME: should the third argument be 'JSString'?  or should we remove this entirely?
foreign import javascript safe
  "$2[$1](JSON.parse($3))"
  js_exec_with_object :: JSString -> JSVal -> JSVal -> IO JSVal

-- | Write a 'JSVal' to stdout (node) or the console (browser). Should only be used for logging wrapped JavaScript objects.
-- The first parameter can be used to briefly describe the log output.
foreign import javascript safe
  "console.log($1, $2);"
  js_console_log_jsval :: JSString -> JSVal -> IO ()

#else

{-# ANN js_exec ("HLint: ignore Use camelCase" :: String) #-}
js_exec :: JSString -> JSVal -> IO JSVal
js_exec = error "javascript FFI not available in GHC"

{-# ANN js_attr ("HLint: ignore Use camelCase" :: String) #-}
js_attr :: JSString -> JSVal -> IO JSVal
js_attr = error "javascript FFI not available in GHC"

{-# ANN js_exec_with_1_arg ("HLint: ignore Use camelCase" :: String) #-}
js_exec_with_1_arg :: JSString -> JSVal -> JSVal -> IO JSVal
js_exec_with_1_arg = error "javascript FFI not available in GHC"

{-# ANN js_exec_with_object ("HLint: ignore Use camelCase" :: String) #-}
js_exec_with_object :: JSString -> JSVal -> JSVal -> IO JSVal
js_exec_with_object = error "javascript FFI not available in GHC"

{-# ANN js_console_log_jsval ("HLint: ignore Use camelCase" :: String) #-}
js_console_log_jsval :: JSString -> JSVal -> IO ()
js_console_log_jsval = error "javascript FFI not available in GHC"

#endif
