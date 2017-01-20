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

import GHCJS.Types (JSString, JSVal, nullRef)
import Control.Monad.IO.Class (liftIO)
import React.Flux
import React.Flux.Internal
import Test.Hspec


spec :: Spec
spec = it "works" $ do
  liftIO $ print "js_somethingGlobalAndUnlikelyNamed"
  liftIO js_somethingGlobalAndUnlikelyNamed
  pending


foreign import javascript unsafe
  "somethingGlobalAndUnlikelyNamed()"
  js_somethingGlobalAndUnlikelyNamed :: IO ()
