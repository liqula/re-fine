module Main where

import Test.Hspec

import qualified Refine.Frontend.CoreSpec as Core


-- FIXME: hspec-discover should be used with GHCJS
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Core.spec
