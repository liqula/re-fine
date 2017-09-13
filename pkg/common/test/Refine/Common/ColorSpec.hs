{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.ColorSpec where
#include "import_common.hs"

import Test.Hspec
import Test.QuickCheck

import Refine.Common.Color
import Refine.Common.Test.Arbitrary ()


spec :: Spec
spec = it "works" pending
