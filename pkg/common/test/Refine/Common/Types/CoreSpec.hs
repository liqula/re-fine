{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.Types.CoreSpec where
#include "import_common.hs"

import           Test.Hspec
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Utils

import Refine.Common.Types.Core
import Refine.Common.Test.Arbitrary

spec :: Spec
spec = parallel $ do
  testBatch $ monoid (mempty :: Ranges Int)

  describe "intersectionRanges" $ do
    it "associative" $ isAssociative (intersectionRanges :: Ranges Int -> Ranges Int -> Ranges Int)
    it "commutative" $ isCommutable (intersectionRanges :: Ranges Int -> Ranges Int -> Ranges Int)
