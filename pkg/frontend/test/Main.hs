{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec
import Spec (spec)
import AllModules ()

#ifdef __GHCJS__
import React.Flux

import Refine.Frontend.Access
import Refine.Frontend.Document.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Store
#endif

main :: IO ()
main = do
#ifdef __GHCJS__
  registerInitialStore emptyGlobalState
  registerInitialStore emptyAccessState
  registerInitialStore emptyEditorStore
  hspec $ before (resetState emptyGlobalState) spec
#else
  hspec spec
#endif
