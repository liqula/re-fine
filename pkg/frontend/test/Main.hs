{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec

#ifdef __GHCJS__
import React.Flux

import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Store
import Spec (spec)
#else
import AllModules ()
#endif


main :: IO ()
main = do
#ifdef __GHCJS__
  registerInitialStore emptyGlobalState
  hspec $ before (resetState emptyGlobalState) spec
#else
  putStrLn "built with GHC -- not running the test suite."

spec :: Spec
spec = it "type-checks!" $ do
  pendingWith "built with GHC -- sensei can check for type errors, but then it'll crash here."
#endif
