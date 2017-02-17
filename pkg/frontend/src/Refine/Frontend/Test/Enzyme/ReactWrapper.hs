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

module Refine.Frontend.Test.Enzyme.ReactWrapper
  ( ReactWrapper
  , mount

  -- * enzyme functions on ReactWrapper
  , module R

  -- * helper functions
  , consoleLogReactWrapper
  ) where

import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

import Refine.Frontend.Test.Enzyme.Class as R
import Refine.Frontend.Test.Enzyme.Core as R
import Refine.Frontend.Test.Enzyme.Class.Internal


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

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

-- | TODO: make this a class methode as well.  it even has a nice default implementation.
consoleLogReactWrapper :: JSString -> ReactWrapper -> IO ()
consoleLogReactWrapper msg (ReactWrapper jsval) = js_console_log_jsval msg jsval

foreign import javascript unsafe
  "enzyme.mount($1)"
  js_mount :: ReactElementRef -> IO JSVal
