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

module Refine.Common.VDoc.HTML.SpliceSpec where

import           Control.Exception (throwIO, ErrorCall(..))
import           Data.String.Conversions ((<>))
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser
import           Text.HTML.Tree

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Splice


spec :: Spec
spec = parallel $ do
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


  describe "chunkCanBeApplied" $ do
    let vers :: VDocVersion 'HTMLCanonical
        vers = VDocVersion "<span data-uid=\"1\">asdfasdf</span>"

        cr :: ChunkPoint -> ChunkRange Patch
        cr p = ChunkRange (ID 3) (Just p) Nothing

        good = ChunkPoint (DataUID 1) 3
        bad1 = ChunkPoint (DataUID 1) 189
        bad2 = ChunkPoint (DataUID 4) 3


    it "returns True on valid chunks." $ do
      chunkRangeCanBeApplied (cr good) vers `shouldBe` True

    it "returns False on out-of-bounds offset." $ do
      chunkRangeCanBeApplied (cr bad1) vers `shouldBe` False

    it "returns False on invalid data-uid." $ do
      chunkRangeCanBeApplied (cr bad2) vers `shouldBe` False


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


  describe "preTokensToForest" $ do
    it "Correctly ignores broken tree structure of PreMarkOpen, PreMarkClose." $ do
      preTokensToForest [PreMarkOpen "1"] `shouldBe` Right [Node (PreMarkOpen "1") []]
      preTokensToForest [PreMarkClose "1"] `shouldBe` Right [Node (PreMarkClose "1") []]
