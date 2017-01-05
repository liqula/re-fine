module Refine.Common.Types.Arbitrary where

import Test.QuickCheck

import Refine.Common.Prelude



instance Arbitrary (ID a) where
  arbitrary     = ID <$> arbitrary
  shrink (ID i) = ID <$> shrink i

