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

module Refine.Frontend.Test.Enzyme.ShallowWrapper
  ( ShallowWrapper
  , shallow

  -- Enzyme functions on a ShallowWrapper
  , shallowChild -- known as "shallow" in Enzyme
  , module R

  -- helper functions
  , consoleLogShallowWrapper
  ) where

import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

import Refine.Frontend.Test.Enzyme.Class as R
import Refine.Frontend.Test.Enzyme.Core as R
import Refine.Frontend.Test.Enzyme.Internal


-- | The ShallowWrapper type.

newtype ShallowWrapper = ShallowWrapper { _unShallowWrapper :: JSVal }
instance PFromJSVal ShallowWrapper where pFromJSVal = ShallowWrapper
instance EnzymeWrapper ShallowWrapper where unWrap = _unShallowWrapper

-- | Functions that only exist for ShallowWrapper.

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

shallow :: ReactElementM eventHandler () -> IO ShallowWrapper
shallow comp = do
  (ref, _) <- mkReactElement (\_ -> pure ()) (ReactThis nullRef) comp
  ShallowWrapper <$> js_shallow ref

shallowChild :: ShallowWrapper -> IO ShallowWrapper
shallowChild = exec "shallow"

-- TODO: simulate(event, data)

-- TODO: dive

-- | Helper functions.

consoleLogShallowWrapper :: JSString -> ShallowWrapper -> IO ()
consoleLogShallowWrapper msg (ShallowWrapper jsval) = js_console_log_jsval msg jsval

foreign import javascript unsafe
  "enzyme.shallow($1)"
  js_shallow :: ReactElementRef -> IO JSVal

