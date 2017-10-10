{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Test.Util where
#include "import_backend.hs"

import           System.IO.Temp (withSystemTempDirectory)
import           System.Directory (withCurrentDirectory)
import           Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Refine.Common.Types


withTempCurrentDirectory :: IO a -> IO a
withTempCurrentDirectory action = withSystemTempDirectory "refine.tmp" (`withCurrentDirectory` action)
