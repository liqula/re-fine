{-# LANGUAGE CPP #-}

module Main where

#ifdef __GHCJS__
import React.Flux
import Test.Hspec

import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Store
import Spec (spec)
#endif


main :: IO ()
main = do
#ifdef __GHCJS__
  registerInitialStore emptyGlobalState
  hspec $ before (resetState emptyGlobalState) spec
#else
  putStrLn "built with GHC -- not running the test suite."
#endif
