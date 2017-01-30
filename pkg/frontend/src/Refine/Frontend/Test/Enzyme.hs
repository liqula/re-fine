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
find = execSW "find"

execSW :: String -> ShallowWrapper -> EnzymeSelector -> IO ShallowWrapper
execSW func (ShallowWrapper wrapper) (StringSelector selector) = do
  ShallowWrapper <$> js_exec_sw_by_string (toJSString func) wrapper (toJSString selector)
execSW func (ShallowWrapper wrapper) (PropertySelector selector) = do
  ShallowWrapper <$> js_exec_sw_by_prop (toJSString func) wrapper ((toJSString . cs) (encode selector))

foreign import javascript unsafe
    "$2[$1]($3)"
    js_exec_sw_by_string :: JSString -> JSVal -> JSString -> IO JSVal

foreign import javascript unsafe
    "$2[$1](JSON.parse($3))"
    js_exec_sw_by_prop :: JSString -> JSVal -> JSString -> IO JSVal

is :: ShallowWrapper -> JSString -> IO Bool
is (ShallowWrapper wrapper) selector = do
  js_is wrapper selector

foreign import javascript unsafe
    "$1.is($2)"
    js_is :: JSVal -> JSString -> IO Bool

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
