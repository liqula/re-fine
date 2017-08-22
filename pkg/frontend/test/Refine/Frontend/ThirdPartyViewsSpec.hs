{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.ThirdPartyViewsSpec where

import Refine.Frontend.Prelude
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.ThirdPartyViews
import Test.Hspec

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

testThirdPartyView :: HasCallStack => ThirdPartyView eventHandler -> Expectation
testThirdPartyView v = testThirdPartyView' (v mempty mempty)

testThirdPartyView' :: HasCallStack => ReactElementM eventHandler () -> Expectation
testThirdPartyView' v = (mount v >>= html) `shouldNotReturn` ""

spec :: Spec
spec = do
  it "skylight_" $ do
    testThirdPartyView skylight_

  it "hammer_" $ do
    pendingWith "uncaught exception: JSException (JavaScript exception: TypeError: Cannot read property '__reactAutoBindPairs' of undefined)"
    testThirdPartyView hammer_

  it "editor_" $ do
    pendingWith "uncaught exception: JSException (JavaScript exception: TypeError: Cannot read property 'getCurrentContent' of undefined)"
    testThirdPartyView editor_
