{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

{-# OPTIONS_GHC -w #-}

module Refine.Common.VDoc.HTMLSpec where

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

import Arbitrary
import Refine.Common.VDoc.HTML


spec :: Spec
spec = do
  describe "canonicalizeWhitespace" $ do
    it "itempotent" . property $
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
        withoutDataUID (TagOpen n xs) = TagOpen n (filter f xs)
          where
            f (Attr "data-uid" _) = False
            f _                   = True
        withoutDataUID t = t

    it "itempotent" . property $
      \s -> setElemUIDs (setElemUIDs s) `shouldBe` setElemUIDs s

    it "existing values are overwritten with integers, starting with 1" $ do
      setElemUIDs [TagOpen "div" [Attr "data-uid" "@@"]] `shouldBe` [TagOpen "div" [Attr "data-uid" "1"]]

    it "data-uid is contained in every tag of the output" . property $ do
      \tokens -> let f t@(TagOpen _ _) = collectDataUID t `shouldSatisfy` ((== 1) . length)
                     f _               = pure ()
                 in f `mapM_` setElemUIDs tokens

    it "data-uid are unique" . property $ do
      \tokens -> let vs :: [ST] = mconcat $ collectDataUID <$> setElemUIDs tokens
                 in vs `shouldBe` []  -- nub vs

    it "everything but data-uid attributes remains unchanged" . property $ do
      \tokens -> (withoutDataUID <$> tokens) `shouldBe` (withoutDataUID <$> setElemUIDs tokens)

  describe "wrapInTopLevelTags" $ do
    it "itempotent" . property . forAll arbitraryTokenForest $
      \forest -> let gotwice, goonce :: [Token] -> Either VDocHTMLError [Token]
                     gotwice          = wrapInTopLevelTags >=> wrapInTopLevelTags
                     goonce           = wrapInTopLevelTags

                     stream = canonicalizeTokens $ tokensFromForest forest
                 in gotwice stream `shouldBe` goonce stream

    it "clothes naked texts in span" $ do
      wrapInTopLevelTags [ContentText "unwrapped"] `shouldBe`
        Right [TagOpen "span" [], ContentText "unwrapped", TagClose "span"]

    it "fails if input is not a valid tree" $ do
      wrapInTopLevelTags [TagOpen "unclosed" []] `shouldBe`
        Left (VDocHTMLErrorBadTree (ParseTokenForestErrorBracketMismatch (PStack [] [(TagOpen "unclosed" [],[])]) Nothing))

    it "crashes (internal error) if input is not canonicalized" $ do
      let bad :: Either VDocHTMLError [Token]
          bad = wrapInTopLevelTags [ContentText "wef", ContentChar 'q']
      evaluate bad `shouldThrow` anyException
