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

module Refine.Frontend.Test.Enzyme.ShallowWrapperAPI
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

import Data.JSString (JSString)
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

import Refine.Frontend.Test.Enzyme.Core

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

shallow :: ReactElementM eventHandler () -> IO ShallowWrapper
shallow comp = do
  (ref, _) <- mkReactElement (\_ -> pure ()) (ReactThis nullRef) comp
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

-- Simulating Events --------------------------------------------------------------------

simulate :: ShallowWrapper -> EventType -> IO ShallowWrapper
simulate = execWith1Arg "simulate"
