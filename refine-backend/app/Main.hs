module Main where

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

import Refine.Backend.Config
import Refine.Backend.Server


main :: IO ()
main = do
  cfg <- initConfig . listToMaybe =<< getArgs
  startBackend cfg
