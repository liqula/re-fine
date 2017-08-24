{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -Wno-orphans #-}

module Refine.Frontend.AccessSpec where

import Test.Hspec
import Refine.Frontend.Access ()
import Refine.Common.Access ()
import Refine.Common.Access.Policy ()

spec :: Spec
spec = it "works" pending
