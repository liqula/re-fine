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
import Data.Aeson (FromJSON, eitherDecode)
import Data.JSString (JSString, unpack)
import Data.String.Conversions (cs)
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal


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

getWrapperAttr :: FromJSON a => ShallowWrapper -> JSString -> IO a
getWrapperAttr (ShallowWrapper wrapper) selector = do
  jsstring <- js_getWrapperAttr wrapper selector
  case eitherDecode . cs . unpack $ jsstring of
    Left e -> throwIO . ErrorCall $ show e
    Right v -> pure v

foreign import javascript unsafe
    "JSON.stringify($1[$2])"
    js_getWrapperAttr :: JSVal -> JSString -> IO JSString
