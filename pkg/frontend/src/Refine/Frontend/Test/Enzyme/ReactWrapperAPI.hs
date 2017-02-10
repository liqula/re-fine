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

module Refine.Frontend.Test.Enzyme.ReactWrapperAPI
( Prop(..)
, ReactWrapper(..)
, EnzymeSelector(..)
, mount
-- Enzyme functions on a ReactWrapper
, at
, childAt
, children
, find
, html
, is
, props
, text
, typeOf

-- JavaScript functions on a ReactWrapper
, lengthOf
, lengthOfIO

-- simulating an event on a ReactWrapper
, simulate
, EventType(..)

) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (ToJSON, toJSON)
import Data.Char (toLower)
import Data.JSString (JSString)
import Data.String.Conversions
import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

newtype ReactWrapper = ReactWrapper JSVal

instance PFromJSVal ReactWrapper where pFromJSVal = ReactWrapper

-- | StringSelector can be a CSS class, tag, id, prop (e.g. "[foo=3]"),
--   component display name. (TODO should this be refined? Enhance? Be checked?)
--   PropertySelector specifies (some of) the element's props
--   ComponentSelector specifies the component constructor (how???)
data EnzymeSelector =
    StringSelector String
  | PropertySelector [Prop]
--  | ComponentSelector

instance PToJSVal EnzymeSelector where
  pToJSVal (StringSelector str) = pToJSVal str
  pToJSVal (PropertySelector p) = pToJSVal . toJSString . cs $ encode p

data Prop where
  Prop :: forall a. (ToJSON a) => ST -> a -> Prop

instance ToJSON [Prop] where
  toJSON = object . fmap (\(Prop k v) -> k .= v)


mount :: ReactElementM eventHandler () -> IO ReactWrapper
mount comp = do
  (ref, _) <- mkReactElement (\_ -> pure ()) (ReactThis nullRef) comp
  ReactWrapper <$> js_mount ref

foreign import javascript unsafe
  "enzyme.mount($1)"
  js_mount :: ReactElementRef -> IO JSVal

find :: ReactWrapper -> EnzymeSelector -> IO ReactWrapper
find = execWithSelector "find"

is :: ReactWrapper -> EnzymeSelector -> IO Bool
is = execWithSelector "is"

childAt :: ReactWrapper -> Int -> IO ReactWrapper
childAt = execWith1Arg "childAt"

at :: ReactWrapper -> Int -> IO ReactWrapper
at = execWith1Arg "at"

props :: ReactWrapper -> IO JSVal
props = exec "props"

typeOf :: ReactWrapper -> IO JSVal
typeOf = exec "type"

children :: ReactWrapper -> IO ReactWrapper
children = exec "children"

html :: ReactWrapper -> IO JSString
html = exec "html"

text :: ReactWrapper -> IO JSString
text = exec "text"

lengthOf :: ReactWrapper -> IO Int
lengthOf = attr "length"

lengthOfIO :: IO ReactWrapper -> IO Int
lengthOfIO wrapper = lengthOf =<< wrapper

-- Simulating Events --------------------------------------------------------------------

data EventType =
    MouseEnter
  | MouseLeave
  deriving (Show)

instance PToJSVal EventType where
  pToJSVal event = let (ch:chs) = show event in pToJSVal (toLower ch : chs)

simulate :: ReactWrapper -> EventType -> IO ReactWrapper
simulate = execWith1Arg "simulate"

-- Preparations for the evaluation of functions in JavaScript --------------------------------------------------

execWithSelector :: PFromJSVal a => String -> ReactWrapper -> EnzymeSelector -> IO a
execWithSelector func (ReactWrapper wrapper) es@(PropertySelector _) = pFromJSVal <$> js_exec_with_object (toJSString func) wrapper (pToJSVal es)
execWithSelector f w e = execWith1Arg f w e

execWith1Arg :: (PFromJSVal a, PToJSVal b) => String -> ReactWrapper -> b -> IO a
execWith1Arg func (ReactWrapper wrapper) arg = pFromJSVal <$> js_exec_with_1_arg (toJSString func) wrapper (pToJSVal arg)

exec :: PFromJSVal a => String -> ReactWrapper -> IO a
exec func (ReactWrapper wrapper) = pFromJSVal <$> js_exec (toJSString func) wrapper

attr :: PFromJSVal a => String -> ReactWrapper -> IO a
attr name (ReactWrapper wrapper) = pFromJSVal <$> js_attr (toJSString name) wrapper

-- The evaluation of functions in JavaScript ----------------------------------

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
