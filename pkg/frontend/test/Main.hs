module Main where

import React.Flux
import Test.Hspec

import Refine.Frontend.Store.Types
import Spec (spec)


main :: IO ()
main = do
  registerInitialStore emptyGlobalState
  hspec spec
