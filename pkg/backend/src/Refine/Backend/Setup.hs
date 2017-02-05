{- | This module is reponsible for setting up the
  context for the server before start.
-}

module Refine.Backend.Setup where

import Control.Lens ((^.))
import System.FilePath (dropFileName)
import System.Directory (createDirectoryIfMissing)

import Refine.Backend.Config


createDataDirectories :: Config -> IO ()
createDataDirectories cfg = do
  createDirectoryIfMissing True (cfg ^. cfgReposRoot)
  case cfg ^. cfgDBKind of
    DBInMemory    -> pure ()
    DBOnDisk path -> createDirectoryIfMissing True (dropFileName path)
