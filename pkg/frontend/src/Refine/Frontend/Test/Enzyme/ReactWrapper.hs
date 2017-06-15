{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Test.Enzyme.ReactWrapper
  ( ReactWrapper
  , mount

  -- * enzyme functions on ReactWrapper
  , module R
  ) where

import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

import Refine.Frontend.Test.Enzyme.Class as R
import Refine.Frontend.Test.Enzyme.Core as R
import Refine.Frontend.Test.Enzyme.Class.Internal


-- * The ReactWrapper type.

newtype ReactWrapper = ReactWrapper { _unReactWrapper :: JSVal }
instance PFromJSVal ReactWrapper where pFromJSVal = ReactWrapper
instance EnzymeWrapper ReactWrapper where unWrap = _unReactWrapper


-- * Functions that only exist for ReactWrapper.

mount :: ReactElementM eventHandler () -> IO ReactWrapper
mount comp = do
  (ref, _) <- mkReactElement (\_ -> pure ()) (ReactThis nullRef) comp
  ReactWrapper <$> js_mount ref

-- TODO: equals

-- TODO: matchesElement

-- TODO: getDOMNode

-- TODO: simulate(event, mock)

-- TODO: mount

-- TODO: matchesElement

-- TODO: ref

-- TODO: detach


-- * Helper functions

#ifdef __GHCJS__

foreign import javascript safe
  "enzyme.mount($1)"
  js_mount :: ReactElementRef -> IO JSVal

#else

{-# ANN js_mount ("HLint: ignore Use camelCase" :: String) #-}
js_mount :: ReactElementRef -> IO JSVal
js_mount = error "javascript FFI not available in GHC"

#endif
