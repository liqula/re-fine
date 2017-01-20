module Main where

import Data.Default (def)
import Refine.Backend.Server

main :: IO ()
main = do
  startBackend def
