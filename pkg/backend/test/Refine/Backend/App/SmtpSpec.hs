{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.App.SmtpSpec where
#include "import_backend.hs"

import           Test.Hspec

import           Refine.Backend.Config ()
import           Refine.Backend.App.Smtp ()

spec :: Spec
spec = it "works" pending
