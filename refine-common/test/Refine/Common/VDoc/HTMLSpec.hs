{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Refine.Common.VDoc.HTMLSpec where

import           Data.Char (isSpace)
import           Data.String.Conversions (ST, cs)
import qualified Data.Text as ST
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import Refine.Common.VDoc.HTML


spec :: Spec
spec = describe "VDocVersion (Forest HTML)" $ do
  describe "canonicalizeWhitespace" $ do
    it "itempotent" . property $
      \s -> canonicalizeWhitespace (canonicalizeWhitespace s) `shouldBe` canonicalizeWhitespace s

    it "all whitespace is newline" . property $
      \(s :: ST) -> (\c -> isSpace c `shouldBe` c == '\n') `mapM_` (cs (canonicalizeWhitespace s) :: String)

    it "leading and trailing whitespace exist in output as in input" . property $
      \s -> do
        let s' = canonicalizeWhitespace s
            check = ST.all isSpace . ST.take 1
        check s `shouldBe` check s'
        check (ST.reverse s) `shouldBe` check (ST.reverse s')
