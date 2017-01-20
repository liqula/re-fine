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

{-# OPTIONS_GHC -w #-}

module Refine.Frontend.EnzymeSpec where  -- TODO: this module probably shouldn't have this name.  it
                                         -- is just a proof of concept for loading js libs into the
                                         -- test suite.

import Control.Monad.IO.Class (liftIO)
import GHCJS.Types (JSString, JSVal, nullRef)
import React.Flux
import React.Flux.Internal
import Test.Hspec

import Refine.Frontend.Store
import Refine.Frontend.Types
import Refine.Frontend.UtilityWidgets
import Refine.Frontend.Views


spec :: Spec
spec = it "works" $ do
  wrapper@(ShallowWrapper jsval) <- shallow $ icon_ "bla"
  js_consoleLog jsval

  wrapper'@(ShallowWrapper jsval') <- find wrapper ".bla"
  js_consoleLog jsval'

  getWrapperAttrInt wrapper' "length" `shouldReturn` 1


newtype ShallowWrapper = ShallowWrapper JSVal

shallow :: ReactElementM eventHandler () -> IO ShallowWrapper
shallow comp = do
  (ref, _) <- mkReactElement (\_ -> return ()) (ReactThis nullRef) comp
  ShallowWrapper <$> js_shallow ref

foreign import javascript unsafe
  "enzyme.shallow($1)"
  js_shallow :: ReactElementRef -> IO JSVal

-- | TODO: move this together with Store.consoleLog into module Util, and import it from there.
-- (the two functions are subtly different: the one in store takes a TOJSON instance, this one takes
-- a JSVal.  we need both as it appears.)
foreign import javascript unsafe
  "console.log($1)"
  js_consoleLog :: JSVal -> IO ()

find :: ShallowWrapper -> JSString -> IO ShallowWrapper
find (ShallowWrapper wrapper) selector = do
  ShallowWrapper <$> js_find wrapper selector

foreign import javascript unsafe
    "$1.find($2)"
    js_find :: JSVal -> JSString -> IO JSVal

-- | TODO: use @FromJSON a => a@ instead of @Int@ in the result.
getWrapperAttrInt :: ShallowWrapper -> JSString -> IO Int
getWrapperAttrInt (ShallowWrapper wrapper) selector = do
  js_getWrapperAttrInt wrapper selector

foreign import javascript unsafe
    "$1[$2]"
    js_getWrapperAttrInt :: JSVal -> JSString -> IO Int
