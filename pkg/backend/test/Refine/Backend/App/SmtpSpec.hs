{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.App.SmtpSpec where
#include "import.hs"

import           Test.Hspec

import           Refine.Backend.Config ()
import           Refine.Backend.App.Smtp ()

spec :: Spec
spec = it "works" pending
