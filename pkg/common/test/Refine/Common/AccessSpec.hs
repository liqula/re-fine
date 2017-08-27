{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.AccessSpec where
#include "import.hs"

import Test.Hspec
import Refine.Common.Access ()
import Refine.Common.Access.Policy ()

spec :: Spec
spec = it "works" pending
