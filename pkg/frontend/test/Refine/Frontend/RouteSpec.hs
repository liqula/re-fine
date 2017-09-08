{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.RouteSpec where
#include "import_frontend.hs"

import Test.Hspec
import Test.QuickCheck as QC

import Refine.Frontend.Route
import Refine.Common.Test.Arbitrary ()


instance Arbitrary Route where
  arbitrary = oneof $
    (pure <$> [Help, Login, Register, Groups, GroupCreate]) <>
    [ GroupProcesses <$> arbitrary
    , GroupMembers <$> arbitrary
    , GroupUpdate <$> arbitrary
    , Process <$> arbitrary
    ]


spec :: Spec
spec = do
  it "rparse . rrender == id" . QC.property $ \r -> rparse (rrender r) `shouldBe` Right r
