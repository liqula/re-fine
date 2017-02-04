{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Test.Enzyme where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Data.Aeson (FromJSON, eitherDecode, encode, object, (.=))
import Data.Aeson.Types (ToJSON, toJSON)
import Data.JSString (JSString, unpack)
import Data.String.Conversions
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal


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
find = execSW_SW "find"

is :: ShallowWrapper -> EnzymeSelector -> IO Bool
is = execB "is"

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

execSW_SW :: String -> ShallowWrapper -> EnzymeSelector -> IO ShallowWrapper
execSW_SW func (ShallowWrapper wrapper) (StringSelector selector) = do
  ShallowWrapper <$> js_exec_sw_sw_by_string (toJSString func) wrapper (toJSString selector)
execSW_SW func (ShallowWrapper wrapper) (PropertySelector selector) = do
  ShallowWrapper <$> js_exec_sw_sw_by_prop (toJSString func) wrapper ((toJSString . cs) (encode selector))

exec_SW :: String -> ShallowWrapper -> IO JSVal
exec_SW func (ShallowWrapper wrapper) = do
  js_exec_sw (toJSString func) wrapper

execI_SW :: String -> ShallowWrapper -> Int -> IO ShallowWrapper
execI_SW func (ShallowWrapper wrapper) index = do
  ShallowWrapper <$> js_exec_i_sw_by_string (toJSString func) wrapper index

execB :: String -> ShallowWrapper -> EnzymeSelector -> IO Bool
execB func (ShallowWrapper wrapper) (StringSelector selector) = do
  js_exec_b_by_string (toJSString func) wrapper (toJSString selector)
execB func (ShallowWrapper wrapper) (PropertySelector selector) = do
  js_exec_b_by_prop (toJSString func) wrapper ((toJSString . cs) (encode selector))

-- TODO unify _sw_ and _b_ by applying the appropriate transformation to the result?
foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_sw_sw_by_string :: JSString -> JSVal -> JSString -> IO JSVal

foreign import javascript unsafe
    "$2[$1]()"
    js_exec_sw :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_i_sw_by_string :: JSString -> JSVal -> Int -> IO JSVal

foreign import javascript unsafe
    "$2[$1](JSON.parse($3))"
    js_exec_sw_sw_by_prop :: JSString -> JSVal -> JSString -> IO JSVal

foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_b_by_string :: JSString -> JSVal -> JSString -> IO Bool

foreign import javascript unsafe
    "$2[$1](JSON.parse($3))"
    js_exec_b_by_prop :: JSString -> JSVal -> JSString -> IO Bool

text :: ShallowWrapper -> IO JSString
text (ShallowWrapper wrapper) = do
  js_text wrapper

foreign import javascript unsafe
    "$1.text()"
    js_text :: JSVal -> IO JSString

length :: ShallowWrapper -> IO Int
length wrapper = getWrapperAttr wrapper "length"

lengthIO :: IO ShallowWrapper -> IO Int
lengthIO wrapper = getIOWrapperAttr wrapper "length"

getIOWrapperAttr :: FromJSON a => IO ShallowWrapper -> JSString -> IO a
getIOWrapperAttr ioWrapper selector = do
  wrapper <- ioWrapper
  getWrapperAttr wrapper selector

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

