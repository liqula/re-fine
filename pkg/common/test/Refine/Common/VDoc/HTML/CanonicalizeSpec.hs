{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.HTML.CanonicalizeSpec where

import           Data.Char (isSpace)
import           Data.Functor.Infix ((<$$>))
import           Data.List (nub)
import           Data.String.Conversions (ST, cs)
import qualified Data.Text as ST
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Canonicalize


spec :: Spec
spec = parallel $ do
  describe "canonicalizeVDocVersion" $ do
    it "decorates spans with data-uid attributes even when added by 'wrapInTopLevelTags'." $ do
      canonicalizeVDocVersion (vdocVersionFromST "ab123")
        `shouldBeVDocVersion` vdocVersionFromST "<span data-uid=\"1\">ab123</span>"

    it "removes comments." $ do
      canonicalizeVDocVersion (vdocVersionFromST "ab<!-- wefij --> 123")
        `shouldBe` vdocVersionFromST "<span data-uid=\"1\">ab\n123</span>"

    it "removes doctype elements." $ do
      canonicalizeVDocVersion (vdocVersionFromST "<span>,</span><QfA] data-uid=\"18\" /><!DOCTYPEW#d>")
        `shouldBe` vdocVersionFromST "<span data-uid=\"1\">,</span><QfA] data-uid=\"2\" />"


  describe "canonicalizeWhitespace" $ do
    it "idempotent" . property $
      \s -> canonicalizeWhitespace (canonicalizeWhitespace s) `shouldBe` canonicalizeWhitespace s

    it "all whitespace is newline" . property $
      \s -> (\c -> isSpace c `shouldBe` c == '\n') `mapM_` cs @ST @String (canonicalizeWhitespace s)

    it "leading and trailing whitespace exist in output as in input" . property $
      \s -> do
        let s' = canonicalizeWhitespace s
            check = ST.all isSpace . ST.take 1
        check s `shouldBe` check s'
        check (ST.reverse s) `shouldBe` check (ST.reverse s')


  describe "setElemUIDs" $ do
    let collectDataUID :: Token -> [ST]
        collectDataUID (TagOpen _ attrs) = mconcat $ f <$> attrs
          where
            f (Attr "data-uid" v) = [v]
            f _                   = []
        collectDataUID _ = []

        withoutDataUID :: Token -> Token
        withoutDataUID = \case (TagOpen n xs)      -> TagOpen n (filter f xs)
                               (TagSelfClose n xs) -> TagSelfClose n (filter f xs)
                               t                   -> t
          where
            f (Attr "data-uid" _) = False
            f _                   = True

    it "idempotent" . property $
      \s -> setElemUIDs (setElemUIDs s) `shouldBe` setElemUIDs s

    it "existing values are overwritten with integers, starting with 1" $ do
      setElemUIDs [TagOpen "div" [Attr "data-uid" "@@"]] `shouldBe` [TagOpen "div" [Attr "data-uid" "1"]]

    it "decorates TagSelfClose" $ do
      setElemUIDs [TagSelfClose "br" []] `shouldBe` [TagSelfClose "br" [Attr "data-uid" "1"]]

    it "data-uid is contained in every tag of the output" . property $ do
      \tokens -> let f t@(TagOpen _ _) = collectDataUID t `shouldSatisfy` ((== 1) . length)
                     f _               = pure ()
                 in f `mapM_` setElemUIDs tokens

    it "data-uid are unique" . property $ do
      \tokens -> let vs :: [ST] = mconcat $ collectDataUID <$> setElemUIDs tokens
                 in vs `shouldBe` nub vs

    it "everything but data-uid attributes remains unchanged" . property $ do
      \tokens -> (withoutDataUID <$> tokens) `shouldBe` (withoutDataUID <$> setElemUIDs tokens)


  describe "wrapInTopLevelTags" $ do
    it "idempotent" . property . forAll arbitraryCanonicalVDocVersion $
      \(VDocVersion forest)
        -> let gotwice = wrapInTopLevelTags . wrapInTopLevelTags
               goonce  = wrapInTopLevelTags
           in gotwice forest `shouldBe` goonce forest

    it "wraps naked texts in span" $ do
      wrapInTopLevelTags [Node (ContentText "unwrapped") []] `shouldBe`
        [Node (TagOpen "span" []) [Node (ContentText "unwrapped") []]]


  describe "canonicalizeAttrs" $ do
    it "takes the first of many values for one key." $ do
      TagOpen "wef" (canonicalizeAttrs [Attr "x" "3", Attr "x" "4"])
        `shouldBe` TagOpen "wef" [Attr "x" "3"]

      TagOpen "wef" (canonicalizeAttrs [Attr "x" "3", Attr "x" "1"])
        `shouldBe` TagOpen "wef" [Attr "x" "3"]

    it "sorts attrs alphabetially." $ do
      TagOpen "wef" (canonicalizeAttrs [Attr "x" "3", Attr "q" "0", Attr "x" "1"])
        `shouldBe` TagOpen "wef" [Attr "q" "0", Attr "x" "3"]

      TagOpen "wef" (canonicalizeAttrs [Attr "x" "3", Attr "q" "9"])
        `shouldBe` TagOpen "wef" [Attr "q" "9", Attr "x" "3"]

      TagOpen "wef" (canonicalizeAttrs [Attr "x" "3", Attr "z" "0"])
        `shouldBe` TagOpen "wef" [Attr "x" "3", Attr "z" "0"]


-- * helpers

-- | Check vdoc versions for equality, and report failures as readable trees.  It runs slower in
-- case of failure, but should have the same run-time cost in case of success.
shouldBeVDocVersion :: VDocVersion a -> VDocVersion a -> Expectation
shouldBeVDocVersion (VDocVersion vers) (VDocVersion vers') =
  if vers == vers'
    then pure ()
    else draw vers `shouldBe` draw vers'
  where
    draw :: Forest Token -> String
    draw f = drawForest $ cs . renderToken <$$> f
