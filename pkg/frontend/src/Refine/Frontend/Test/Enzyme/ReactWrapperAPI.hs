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
( mount
, module R
) where

import GHCJS.Types (JSVal, nullRef)
import React.Flux
import React.Flux.Internal

import Refine.Frontend.Test.Enzyme.CommonAPI as R
import Refine.Frontend.Test.Enzyme.Core as R

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

mount :: ReactElementM eventHandler () -> IO ReactWrapper
mount comp = do
  (ref, _) <- mkReactElement (\_ -> pure ()) (ReactThis nullRef) comp
  ReactWrapper <$> js_mount ref

foreign import javascript unsafe
  "enzyme.mount($1)"
  js_mount :: ReactElementRef -> IO JSVal

-- TODO: equals

-- TODO: matchesElement

-- TODO: getDOMNode

-- TODO: simulate(event, mock)

-- TODO: mount

-- TODO: matchesElement

-- TODO: ref

-- TODO: detach
