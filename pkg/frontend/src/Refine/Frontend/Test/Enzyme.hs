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

module Refine.Frontend.Test.Enzyme
( Prop(..)
, ShallowWrapper(..)
, EnzymeSelector(..)
, shallow
-- Enzyme functions on a ShallowWrapper
, at
, childAt
, children
, find
, html
, is
, props
, shallowChild -- known as "shallow" in Enzyme
, text
, typeOf

-- JavaScript functions on a ShallowWrapper
, lengthOf
, lengthOfIO

-- simulating an event on a ShallowWrapper
, simulate
, EventType(..)

) where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Data.Aeson (FromJSON, eitherDecode, encode, object, (.=))
import Data.Aeson.Types (ToJSON, toJSON)
import Data.JSString (JSString, unpack)
import Data.String.Conversions
import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

newtype ShallowWrapper = ShallowWrapper JSVal

-- | StringSelector can be a CSS class, tag, id, prop (e.g. "[foo=3]"),
--   component display name. (TODO should this be refined? Enhance? Be checked?)
--   PropertySelector specifies (some of) the element's props
--   ComponentSelector specifies the component constructor (how???)
data EnzymeSelector =
    StringSelector String
  | PropertySelector [Prop]
--  | ComponentSelector


data Prop where
  Prop :: forall a. (ToJSON a) => ST -> a -> Prop

instance ToJSON [Prop] where
  toJSON = object . fmap (\(Prop k v) -> k .= v)


shallow :: ReactElementM eventHandler () -> IO ShallowWrapper
shallow comp = do
  (ref, _) <- mkReactElement (\_ -> return ()) (ReactThis nullRef) comp
  ShallowWrapper <$> js_shallow ref

foreign import javascript unsafe
  "enzyme.shallow($1)"
  js_shallow :: ReactElementRef -> IO JSVal

find :: ShallowWrapper -> EnzymeSelector -> IO ShallowWrapper
find = execWithSelector "find" ShallowWrapper

is :: ShallowWrapper -> EnzymeSelector -> IO Bool
is = execWithSelector "is" pFromJSVal

childAt :: ShallowWrapper -> Int -> IO ShallowWrapper
childAt = execI_SW "childAt"

at :: ShallowWrapper -> Int -> IO ShallowWrapper
at = execI_SW "at"

props :: ShallowWrapper -> IO JSVal
props = exec_SW "props"

typeOf :: ShallowWrapper -> IO JSVal
typeOf = exec_SW "type"

shallowChild :: ShallowWrapper -> IO ShallowWrapper
shallowChild wrapper = ShallowWrapper <$> exec_SW "shallow" wrapper

children :: ShallowWrapper -> IO ShallowWrapper
children wrapper = ShallowWrapper <$> exec_SW "children" wrapper

html :: ShallowWrapper -> IO JSString
html = exec_SW_Str "html"

text :: ShallowWrapper -> IO JSString
text = exec_SW_Str "text"

execWithSelector :: String -> (JSVal -> a) -> ShallowWrapper -> EnzymeSelector -> IO a
execWithSelector func conv (ShallowWrapper wrapper) (StringSelector selector)   = conv <$> js_exec_with_string (toJSString func) wrapper (toJSString selector)
execWithSelector func conv (ShallowWrapper wrapper) (PropertySelector selector) = conv <$> js_exec_with_object (toJSString func) wrapper ((toJSString . cs) (encode selector))


exec_SW :: String -> ShallowWrapper -> IO JSVal
exec_SW func (ShallowWrapper wrapper) = do
  js_exec_sw (toJSString func) wrapper

exec_SW_Str :: String -> ShallowWrapper -> IO JSString
exec_SW_Str func (ShallowWrapper wrapper) = do
  js_exec_sw_str (toJSString func) wrapper

execI_SW :: String -> ShallowWrapper -> Int -> IO ShallowWrapper
execI_SW func (ShallowWrapper wrapper) index = do
  ShallowWrapper <$> js_exec_i_sw_by_string (toJSString func) wrapper index


------------------------------------
foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_with_string :: JSString -> JSVal -> JSString -> IO JSVal

foreign import javascript unsafe
    "$2[$1](JSON.parse($3))"
    js_exec_with_object :: JSString -> JSVal -> JSString -> IO JSVal

------------------------------------

foreign import javascript unsafe
    "$2[$1]()"
    js_exec_sw :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$2[$1]()"
    js_exec_sw_str :: JSString -> JSVal -> IO JSString

foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_i_sw_by_string :: JSString -> JSVal -> Int -> IO JSVal

lengthOf :: ShallowWrapper -> IO Int
lengthOf wrapper = getWrapperAttr wrapper "length"

lengthOfIO :: IO ShallowWrapper -> IO Int
lengthOfIO wrapper = lengthOf =<< wrapper

getWrapperAttr :: FromJSON a => ShallowWrapper -> JSString -> IO a
getWrapperAttr (ShallowWrapper wrapper) selector = do
  jsstring <- js_getWrapperAttr wrapper selector
  case eitherDecode . cs . unpack $ jsstring of
    Left e -> throwIO . ErrorCall $ show e
    Right v -> pure v

foreign import javascript unsafe
    "JSON.stringify($1[$2])"
    js_getWrapperAttr :: JSVal -> JSString -> IO JSString

data EventType =
    MouseEnter
  | MouseLeave
  deriving (Show)

simulate :: ShallowWrapper -> EventType -> IO ShallowWrapper
simulate (ShallowWrapper wrapper) event = ShallowWrapper <$> js_simulate wrapper (toJSString (show event))



foreign import javascript unsafe
    "$1.simulate($2)"
    js_simulate :: JSVal -> JSString -> IO JSVal

