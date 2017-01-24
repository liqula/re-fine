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

import           Control.Exception (evaluate)
import           Control.Monad ((>=>))
import           Data.Char (isSpace)
import           Data.List (nub)
import           Data.String.Conversions (ST, cs)
import qualified Data.Text as ST
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser
import           Text.HTML.Tree

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Common.VDoc.HTML.Core


spec :: Spec
spec = parallel $ do
  describe "canonicalizeVDocVersion" $ do
    it "decorates spans with data-uid attributes even when added by 'wrapInTopLevelTags'." $ do
      canonicalizeVDocVersion (VDocVersion "ab123")
        `shouldBe` Right (VDocVersion "<span data-uid=\"1\">ab123</span>")

    it "removes comments." $ do
      canonicalizeVDocVersion (VDocVersion "ab<!-- wefij --> 123")
        `shouldBe` Right (VDocVersion "<span data-uid=\"1\">ab\n123</span>")

    it "removes doctype elements." $ do
      canonicalizeVDocVersion (VDocVersion "<span>,</span><QfA] data-uid=\"18\" /><!DOCTYPEW#d>")
        `shouldBe` Right (VDocVersion "<span data-uid=\"1\">,</span><QfA] data-uid=\"2\" />")


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
    it "idempotent" . property . forAll arbitraryCanonicalTokenStream $
      \stream -> let gotwice, goonce :: [Token] -> Either VDocHTMLError [Token]
                     gotwice          = wrapInTopLevelTags >=> wrapInTopLevelTags
                     goonce           = wrapInTopLevelTags
                 in gotwice stream `shouldBe` goonce stream

    it "wraps naked texts in span" $ do
      wrapInTopLevelTags [ContentText "unwrapped"] `shouldBe`
        Right [TagOpen "span" [], ContentText "unwrapped", TagClose "span"]

    it "fails if input is not a valid tree" $ do
      wrapInTopLevelTags [TagOpen "unclosed" []] `shouldBe`
        Left (VDocHTMLErrorBadTree (ParseTokenForestErrorBracketMismatch (PStack [] [(TagOpen "unclosed" [],[])]) Nothing))

    it "crashes (internal error) if input is not canonicalized" $ do
      pendingWith "#16"
      let bad :: Either VDocHTMLError [Token]
          bad = wrapInTopLevelTags [ContentText "wef", ContentChar 'q']
      evaluate bad `shouldThrow` anyException


  describe "canonicalizeAttrs" $ do
    it "takes the first of many values for one key." $ do
      canonicalizeAttrs (TagOpen "wef" [Attr "x" "3", Attr "x" "4"])
        `shouldBe` TagOpen "wef" [Attr "x" "3"]

      canonicalizeAttrs (TagOpen "wef" [Attr "x" "3", Attr "x" "1"])
        `shouldBe` TagOpen "wef" [Attr "x" "3"]

    it "sorts attrs alphabetially." $ do
      canonicalizeAttrs (TagOpen "wef" [Attr "x" "3", Attr "q" "0", Attr "x" "1"])
        `shouldBe` TagOpen "wef" [Attr "q" "0", Attr "x" "3"]

      canonicalizeAttrs (TagOpen "wef" [Attr "x" "3", Attr "q" "9"])
        `shouldBe` TagOpen "wef" [Attr "q" "9", Attr "x" "3"]

      canonicalizeAttrs (TagOpen "wef" [Attr "x" "3", Attr "z" "0"])
        `shouldBe` TagOpen "wef" [Attr "x" "3", Attr "z" "0"]
