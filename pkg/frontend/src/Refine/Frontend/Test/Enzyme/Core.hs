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

module Refine.Frontend.Test.Enzyme.Core
( Prop(..)
, EventType(..)
, EnzymeSelector(..)

, EnzymeWrapper
, ShallowWrapper (..) -- TODO is it good practice to make the constructors accessible? Or should we rather hide them in here?
, ReactWrapper (..)

, execWith1Arg
, exec
, attr
) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (ToJSON, toJSON)
import Data.Char (toLower)
import Data.String.Conversions
import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal)
import React.Flux.Internal

import Refine.Frontend.Test.Enzyme.Internal


-- | StringSelector can be a CSS class, tag, id, prop (e.g. "[foo=3]"),
--   component display name. (TODO should this be refined? Enhance? Be checked?)
--   PropertySelector specifies (some of) the element's props
--   ComponentSelector specifies the component constructor (how???)
data EnzymeSelector =
    StringSelector String
  | PropertySelector [Prop]
--  | ComponentSelector

-- | TODO: 'PropertySelector' should be translated to js array, then we could get rid of
-- 'js_exec_with_object', and 'execWithSelector' as a special case of 'execWith1Arg'.
instance PToJSVal EnzymeSelector where
  pToJSVal (StringSelector str) = pToJSVal str
  pToJSVal (PropertySelector p) = pToJSVal . toJSString . cs $ encode p

data EventType =
    MouseEnter
  | MouseLeave
  | Click
  deriving (Show)

-- | Turns the event into a string where the first character is lowercased. (Note: This is required for the ReactWrapper)
instance PToJSVal EventType where
  pToJSVal event = let (ch:chs) = show event in pToJSVal (toLower ch : chs)

data Prop where
  Prop :: forall a. (ToJSON a) => ST -> a -> Prop

instance ToJSON [Prop] where
  toJSON = object . fmap (\(Prop k v) -> k .= v)

newtype ShallowWrapper = ShallowWrapper { _unShallowWrapper :: JSVal }
instance PFromJSVal ShallowWrapper where pFromJSVal = ShallowWrapper

newtype ReactWrapper = ReactWrapper { _unReactWrapper :: JSVal }
instance PFromJSVal ReactWrapper where pFromJSVal = ReactWrapper

class EnzymeWrapper a where
  unWrap :: a -> JSVal

instance EnzymeWrapper ShallowWrapper where unWrap = _unShallowWrapper
instance EnzymeWrapper ReactWrapper where unWrap = _unReactWrapper

execWith1Arg :: (PFromJSVal a, PToJSVal b, EnzymeWrapper w) => String -> w -> b -> IO a
execWith1Arg func wrapper = execWith1Arg' func $ unWrap wrapper
  where
  execWith1Arg' :: (PFromJSVal a, PToJSVal b) => String -> JSVal -> b -> IO a
  execWith1Arg' func' wrapper' arg' = pFromJSVal <$> js_exec_with_1_arg (toJSString func') wrapper' (pToJSVal arg')

exec :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
exec func wrapper = exec' func $ unWrap wrapper
  where
  exec' :: PFromJSVal a => String -> JSVal -> IO a
  exec' func' wrapper' = pFromJSVal <$> js_exec (toJSString func') wrapper'

attr :: (PFromJSVal a, EnzymeWrapper w) => String -> w -> IO a
attr name wrapper = attr' name $ unWrap wrapper
  where
  attr' :: PFromJSVal a => String -> JSVal -> IO a
  attr' name' wrapper' = pFromJSVal <$> js_attr (toJSString name') wrapper'
