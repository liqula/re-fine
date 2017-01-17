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

module Refine.Common.VDoc.HTMLSpec where

import           Control.Exception (evaluate, throwIO, ErrorCall(..))
import           Control.Monad ((>=>))
import           Data.Char (isSpace)
import           Data.List (nub)
import           Data.String.Conversions (ST, cs, (<>))
import qualified Data.Text as ST
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser
import           Text.HTML.Tree

import Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Common.VDoc.HTML.Splice


spec :: Spec
spec = do
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
        withoutDataUID (TagOpen n xs) = TagOpen n (filter f xs)
          where
            f (Attr "data-uid" _) = False
            f _                   = True
        withoutDataUID t = t

    it "idempotent" . property $
      \s -> setElemUIDs (setElemUIDs s) `shouldBe` setElemUIDs s

    it "existing values are overwritten with integers, starting with 1" $ do
      setElemUIDs [TagOpen "div" [Attr "data-uid" "@@"]] `shouldBe` [TagOpen "div" [Attr "data-uid" "1"]]

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


  describe "trickledownUIInfo" $ do
    it "trickles down data-uid, offset" $ do
      let ts  = [ TagOpen "div" [Attr "data-uid" "3"]
                , TagOpen "div" []
                , TagClose "div"
                , TagClose "div"
                ]
          ts' = [ TagOpen "div" [Attr "data-uid" "3", Attr "data-offset" "0"]
                , TagOpen "div" [Attr "data-uid" "3", Attr "data-offset" "0"]
                , TagClose "div"
                , TagClose "div"
                ]
      (canonicalizeAttrsForest . trickledownUIInfo <$> tokensToForest ts) `shouldBe`
        (canonicalizeAttrsForest <$> tokensToForest ts')

    it "does not overwrite data-uid" $ do
      let ts  = [ TagOpen "div" [Attr "data-uid" "3"]
                , TagOpen "div" [Attr "data-uid" "9"]
                , TagClose "div"
                , TagClose "div"
                ]
          ts' = [ TagOpen "div" [Attr "data-uid" "3", Attr "data-offset" "0"]
                , TagOpen "div" [Attr "data-uid" "9", Attr "data-offset" "0"]
                , TagClose "div"
                , TagClose "div"
                ]
      (canonicalizeAttrsForest . trickledownUIInfo <$> tokensToForest ts) `shouldBe`
        (canonicalizeAttrsForest <$> tokensToForest ts')

    it "calculates offset correctly (flat sibling text nodes)" $ do
      let ts  = [ TagOpen "div" [Attr "data-uid" "3"]
                , ContentText "ab"
                , TagOpen "div" [Attr "data-uid" "9"]
                , TagClose "div"
                , TagClose "div"
                ]
          ts' = [ TagOpen "div" [Attr "data-uid" "3", Attr "data-offset" "0"]
                , ContentText "ab"
                , TagOpen "div" [Attr "data-uid" "9", Attr "data-offset" (cs . show . length $ ("ab" :: String))]
                , TagClose "div"
                , TagClose "div"
                ]
      (canonicalizeAttrsForest . trickledownUIInfo <$> tokensToForest ts) `shouldBe`
        (canonicalizeAttrsForest <$> tokensToForest ts')

    it "calculates offset correctly (sibling trees)" $ do
      let ts  = [ TagOpen "div" [Attr "data-uid" "3"]
                , ContentText "abc"
                , TagOpen "div" [Attr "data-uid" "4"]
                , ContentText "ab"
                , TagClose "div"
                , TagOpen "div" [Attr "data-uid" "5"]
                , TagClose "div"
                , TagClose "div"
                ]
          ts' = [ TagOpen "div" [Attr "data-uid" "3", Attr "data-offset" "0"]
                , ContentText "abc"
                , TagOpen "div" [Attr "data-uid" "4", Attr "data-offset" "3"]  -- TODO: 5
                , ContentText "ab"
                , TagClose "div"
                , TagOpen "div" [Attr "data-uid" "5", Attr "data-offset" (cs . show . length $ ("abc" <> "ab" :: String))]  -- TODO: 0
                , TagClose "div"
                , TagClose "div"
                ]
      (canonicalizeAttrsForest . trickledownUIInfo <$> tokensToForest ts) `shouldBe`
        (canonicalizeAttrsForest <$> tokensToForest ts')


  describe "insertMarks" $ do
    it "does not change version if chunk list is empty." . property . forAll arbitraryCanonicalVDocVersion $ do
      \vers -> do
        _unVDocVersion <$> insertMarks [] vers `shouldBe` Right (_unVDocVersion vers)

    it "generates valid output on arbitrary valid chunkranges." $ do
      pending

    it "marks are inserted under the correct parent node." $ do
      pending

    it "crashes (assertion failed) if input is not canonicalized" $ do
      let eval :: Show e => Either e a -> IO a
          eval = either (throwIO . ErrorCall . show) pure

          bad :: Either VDocHTMLError (VDocVersion 'HTMLWithMarks)
          bad = insertMarks [] $ VDocVersion "<wef>q"  -- (data-uid attr. missing in tag)

      eval bad `shouldThrow` anyException


  describe "resolvePreTokens" $ do
    let runPreTokenForest :: Forest PreToken -> [Token]
        runPreTokenForest = tokensFromForest . fmap (fmap runPreToken)

    context "w/o PreMarks" $ do
      it "inverts enablePreTokens" . property . forAll arbitraryCanonicalTokenForest $ do
        \(forest :: Forest Token) -> do
          let go   :: [Token] = runPreTokenForest $ enablePreTokens forest
              stay :: [Token] = tokensFromForest forest
          go `shouldBe` stay

    context "with consistent PreMarks" $ do
      it "removes empty selections" $ do
        resolvePreTokens [PreMarkOpen "2", PreMarkClose "2"]
          `shouldBe` Right []

      it "keeps selections that have only tags in them, but no text" $ do
        -- (not sure this is what we want, but it's what we currently do!)
        resolvePreTokens [PreMarkOpen "2", PreMarkOpen "8", PreMarkClose "8", PreMarkClose "2"]
          `shouldBe` Right [TagOpen "mark" [Attr "data-chunk-id" "2"], TagClose "mark"]

      it "renders marks as tags" $ do
        resolvePreTokens [PreMarkOpen "2", PreToken $ ContentText "wef", PreMarkClose "2"]
          `shouldBe` Right [TagOpen "mark" [Attr "data-chunk-id" "2"], ContentText "wef", TagClose "mark"]
        resolvePreTokens [ PreMarkOpen "2"
                         , PreToken $ ContentText "wef"
                         , PreMarkOpen "8"
                         , PreToken $ ContentText "puh"
                         , PreMarkClose "8"
                         , PreMarkClose "2"
                         ]
          `shouldBe`
               Right [ TagOpen "mark" [Attr "data-chunk-id" "2"]
                     , ContentText "wef"
                     , TagOpen "mark" [Attr "data-chunk-id" "8"]
                     , ContentText "puh"
                     , TagClose "mark"
                     , TagClose "mark"
                     ]

    context "with *in*consistent PreMarks" $ do
      it "fails" $ do
        let bad1 = [ PreMarkOpen "2"
                   ]
            bad2 = [ PreMarkOpen "2"
                   , PreMarkOpen "8"
                   , PreMarkClose "2"
                   , PreMarkClose "8"
                   ]

        resolvePreTokens bad1
          `shouldBe` Left ("resolvePreTokens: open without close: " <> show ([PreMarkOpen "2"], bad1))
        resolvePreTokens bad2
          `shouldBe` Left ("resolvePreTokens: close without open: " <> show ([PreMarkClose "8"], bad2))
