module Main where

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)

import Refine.Backend.Config
import Refine.Backend.Server


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  cfg <- initConfig . listToMaybe =<< getArgs
  startBackend cfg
