{-# LANGUAGE CPP #-}
#include "language.hs"
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test (module P, passes, failsOn) where

import Refine.Common.Prelude

import Test.Hspec as P
import Test.QuickCheck as P
import "quickcheck-instances" Test.QuickCheck.Instances as P ()
import Test.QuickCheck.Monadic as P

import Refine.Common.Test.Arbitrary as P
import Refine.Common.Test.HttpApiData as P
import Refine.Common.Test.Samples as P


passes :: HasCallStack => Expectation
passes = True `shouldBe` True

failsOn :: HasCallStack => Show a => a -> Expectation
failsOn a = show a `shouldBe` "something else"
