{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.AccessSpec where
#include "import_common.hs"

import Test.Hspec
import Refine.Common.Access ()
import Refine.Common.Access.Policy ()

spec :: Spec
spec = it "works" pending
