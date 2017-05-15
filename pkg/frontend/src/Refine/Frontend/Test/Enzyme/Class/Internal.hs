{-# LANGUAGE NoImplicitPrelude          #-}
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

-- TODO: findWhere

-- TODO: filter

-- TODO: filterWhere

-- TODO: contains

-- TODO: containsMatchingElement

-- TODO: containsAllMatchingElements

-- TODO: containsAnyMatchingElements

-- TODO: hasClass

is :: EnzymeWrapper w => w -> EnzymeSelector -> IO Bool
is = execWithSelector "is"

-- TODO: exists

-- TODO: isEmpty

-- TODO: not

children :: EnzymeWrapper w => w -> IO w
children = exec "children"

childAt :: EnzymeWrapper w => w -> Int -> IO w
childAt = execWith1Arg "childAt"

-- TODO: parents

-- TODO: parent

-- TODO: closest

-- TODO: render
-- TODO: CheerioWrapper

text :: EnzymeWrapper w => w -> IO JSString
text = exec "text"

html :: EnzymeWrapper w => w -> IO JSString
html = exec "html"

-- TODO: get

-- TODO: getNode

-- TODO: getNodes

at :: EnzymeWrapper w => w -> Int -> IO w
at = execWith1Arg "at"

-- TODO: first

-- TODO: last

-- TODO: state

-- TODO: context

props :: EnzymeWrapper w => w -> IO JSVal
props = exec "props"

-- TODO: prop

-- TODO: key

simulate :: EnzymeWrapper w => w -> EventType -> IO ()
simulate = execWith1Arg "simulate"

-- TODO: setState

-- TODO: setProps

-- TODO: setContext

-- TODO: instance

-- TODO: unmount

-- TODO: update

-- TODO: debug

-- | "type" in AirBnB Enzyme.
typeOf :: EnzymeWrapper w => w -> IO JSVal
typeOf = exec "type"

-- TODO: name

-- TODO: forEach

-- TODO: map

-- TODO: reduce

-- TODO: reduceRight

-- TODO: slice

-- TODO: tap

-- TODO: some

-- TODO: someWhere

-- TODO: every

-- TODO: everyWhere


-- * Helper Functions

lengthOf :: EnzymeWrapper w => w -> IO Int
lengthOf = attr "length"

lengthOfIO :: EnzymeWrapper w => IO w -> IO Int
lengthOfIO wrapper = lengthOf =<< wrapper


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- * Preparations for the evaluation of functions in JavaScript

execWithSelector :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> EnzymeSelector -> IO a
execWithSelector func wrapper es@(PropertySelector _) = pFromJSVal <$> js_exec_with_object (pack func) (unWrap wrapper) (pToJSVal es)
execWithSelector f w e = execWith1Arg f w e

execWith1Arg :: (PFromJSVal a, PToJSVal b, EnzymeWrapper w) => String -> w -> b -> IO a
execWith1Arg func wrapper arg = pFromJSVal <$> js_exec_with_1_arg (pack func) (unWrap wrapper) (pToJSVal arg)

exec :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
exec func wrapper = pFromJSVal <$> js_exec (pack func) (unWrap wrapper)

attr :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
attr name wrapper = pFromJSVal <$> js_attr (pack name) (unWrap wrapper)


-- * The actual JavaScript calls

foreign import javascript unsafe
  "$2[$1]()"
  js_exec :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
  "$2[$1]"
  js_attr :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
  "$2[$1]($3)"
  js_exec_with_1_arg :: JSString -> JSVal -> JSVal -> IO JSVal

-- | Log objects that have no 'PToJSVal' instance, but a 'ToJSON' instance, and thus can be
-- conveniently turned into a JSString.  (This is neither efficient nor pretty, but arguably it's
-- good enough to write useful tests.)
--
-- TODO: should the third argument be 'JSString'?  or should we remove this entirely?
foreign import javascript unsafe
  "$2[$1](JSON.parse($3))"
  js_exec_with_object :: JSString -> JSVal -> JSVal -> IO JSVal

-- | Write a 'JSVal' to stdout (node) or the console (browser). Should only be used for logging wrapped JavaScript objects.
-- The first parameter can be used to briefly describe the log output.
foreign import javascript unsafe
  "console.log($1, $2);"
  js_console_log_jsval :: JSString -> JSVal -> IO ()
