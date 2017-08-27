{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.ThirdPartyViewsSpec where
#include "import_frontend.hs"

import Test.Hspec
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.ThirdPartyViews

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
