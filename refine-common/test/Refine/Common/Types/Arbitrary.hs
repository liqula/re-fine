{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Types.Arbitrary where

import Test.QuickCheck

import Refine.Common.Types.Prelude



instance Arbitrary (ID a) where
  arbitrary     = ID <$> arbitrary
  shrink (ID i) = ID <$> shrink i
