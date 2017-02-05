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

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (ToJSON, toJSON)
import Data.JSString (JSString)
import Data.String.Conversions
import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

newtype ShallowWrapper = ShallowWrapper JSVal

instance PFromJSVal ShallowWrapper where pFromJSVal = ShallowWrapper

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


shallow :: ReactElementM eventHandler () -> IO ShallowWrapper
shallow comp = do
  (ref, _) <- mkReactElement (\_ -> return ()) (ReactThis nullRef) comp
  ShallowWrapper <$> js_shallow ref

foreign import javascript unsafe
  "enzyme.shallow($1)"
  js_shallow :: ReactElementRef -> IO JSVal

find :: ShallowWrapper -> EnzymeSelector -> IO ShallowWrapper
find = execWithSelector "find"

is :: ShallowWrapper -> EnzymeSelector -> IO Bool
is = execWithSelector "is"

childAt :: ShallowWrapper -> Int -> IO ShallowWrapper
childAt = execWith1Arg "childAt"

at :: ShallowWrapper -> Int -> IO ShallowWrapper
at = execWith1Arg "at"

props :: ShallowWrapper -> IO JSVal
props = exec "props"

typeOf :: ShallowWrapper -> IO JSVal
typeOf = exec "type"

shallowChild :: ShallowWrapper -> IO ShallowWrapper
shallowChild = exec "shallow"

children :: ShallowWrapper -> IO ShallowWrapper
children = exec "children"

html :: ShallowWrapper -> IO JSString
html = exec "html"

text :: ShallowWrapper -> IO JSString
text = exec "text"

lengthOf :: ShallowWrapper -> IO Int
lengthOf = attr "length"

lengthOfIO :: IO ShallowWrapper -> IO Int
lengthOfIO wrapper = lengthOf =<< wrapper

-- Preparations for the evaluation of functions in JavaScript --------------------------------------------------

execWithSelector :: PFromJSVal a => String -> ShallowWrapper -> EnzymeSelector -> IO a
execWithSelector func (ShallowWrapper wrapper) es@(PropertySelector _) = pFromJSVal <$> js_exec_with_object (toJSString func) wrapper (pToJSVal es)
execWithSelector f w e = execWith1Arg f w e

execWith1Arg :: (PFromJSVal a, PToJSVal b) => String -> ShallowWrapper -> b -> IO a
execWith1Arg func (ShallowWrapper wrapper) num = pFromJSVal <$> js_exec_with_1_arg (toJSString func) wrapper (pToJSVal num)

exec :: PFromJSVal a => String -> ShallowWrapper -> IO a
exec func (ShallowWrapper wrapper) = pFromJSVal <$> js_exec (toJSString func) wrapper

attr :: PFromJSVal a => String -> ShallowWrapper -> IO a
attr name (ShallowWrapper wrapper) = pFromJSVal <$> js_attr (toJSString name) wrapper

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

-- Simulating Events --------------------------------------------------------------------

data EventType =
    MouseEnter
  | MouseLeave
  deriving (Show)

simulate :: ShallowWrapper -> EventType -> IO ShallowWrapper
simulate (ShallowWrapper wrapper) event = ShallowWrapper <$> js_simulate wrapper (toJSString (show event))

foreign import javascript unsafe
    "$1.simulate($2)"
    js_simulate :: JSVal -> JSString -> IO JSVal

