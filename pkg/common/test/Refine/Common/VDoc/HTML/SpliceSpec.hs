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

import           Control.Exception (throwIO, ErrorCall(..), evaluate)
import           Data.String.Conversions ((<>))
import           Data.Tree
import           Data.Either (isRight)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser
import           Text.HTML.Tree

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Splice


noChunkRanges :: [ChunkRange ()]
noChunkRanges = []


spec :: Spec
spec = parallel $ do
  describe "insertMarks" $ do
    it "does not change version if chunk list is empty." . property . forAll arbitraryCanonicalVDocVersion $ do
      \vers -> do
        _unVDocVersion <$> insertMarks noChunkRanges vers `shouldBe` Right (_unVDocVersion vers)

    it "self-closing tags with closing `/`." $ do
      let vers = VDocVersion "<div data-uid=\"1\"><br data-uid=\"2\"/>el</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 0))
                                                   (Just (ChunkPoint (DataUID 1) 1))
      chunkRangeMismatch r vers `shouldBe` []
      insertMarks [r] vers `shouldSatisfy` isRight

    it "fails on self-closing tags without closing `/` (should be resolved by canonicalization)." $ do
      pendingWith "#16"
      let vers = VDocVersion "<div data-uid=\"1\"><br>el</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 0))
                                                   (Just (ChunkPoint (DataUID 1) 1))
      (evaluate . length . show $ chunkRangeMismatch r vers) `shouldThrow` anyException

    it "regression (1)." $ do
      let vers = VDocVersion "<span data-uid=\"1\">whee</span><div O=\"\" data-uid=\"2\"></div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 3))
                                                   (Just (ChunkPoint (DataUID 2) 0))
      chunkRangeCanBeApplied r vers `shouldBe` True
      insertMarks [r] vers `shouldSatisfy` isRight

    it "regression (2)." $ do
      let vers = VDocVersion "<span data-uid=\"1\">whee</span><div O=\"\" data-uid=\"2\">.</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 3))
                                                   (Just (ChunkPoint (DataUID 2) 0))
      chunkRangeCanBeApplied r vers `shouldBe` True
      insertMarks [r] vers `shouldSatisfy` isRight

    it "regression (3)." $ do
      let vers = VDocVersion "<span data-uid=\"1\">whee</span>"
          r :: ChunkRange Edit = ChunkRange (ID 3) (Just (ChunkPoint (DataUID 1) 2)) Nothing
      chunkRangeCanBeApplied r vers `shouldBe` True
      insertMarks [r] vers `shouldSatisfy` isRight

    it "regression (4)." $ do
      let vers = VDocVersion "<span data-uid=\"1\">whee</span>"
          rs :: [ChunkRange Edit] = [ ChunkRange (ID 3) (Just (ChunkPoint (DataUID 1) 2)) Nothing
                                    , ChunkRange (ID 4) (Just (ChunkPoint (DataUID 1) 0)) Nothing
                                    ]
      insertMarks rs vers `shouldSatisfy` isRight

    it "regression (5)." $ do
      let vers = VDocVersion "<span data-uid=\"4\">zC9E</span><n data-uid=\"5\"><f data-uid=\"6\"></f>;</n><T data-uid=\"7\"></T><i data-uid=\"8\"></i>"
          rs :: [ChunkRange Edit]
          rs = [ ChunkRange (ID 0) (Just (ChunkPoint (DataUID 4) 2)) (Just (ChunkPoint (DataUID 6) 0))
               , ChunkRange (ID 2) Nothing                           (Just (ChunkPoint (DataUID 5) 0))
               ]

          empty :: ChunkRange Edit
          empty = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 5) 1)) (Just (ChunkPoint (DataUID 6) 0))

      chunkRangeMismatch empty vers `shouldNotSatisfy` null
      insertMarks rs vers `shouldSatisfy` isRight

    it "generates valid output on arbitrary valid chunkranges." . property $ do
      \(VersWithRanges vers rs) -> do
        insertMarks rs vers `shouldSatisfy` isRight

    it "marks are inserted under the correct parent node." $ do
      pending

    it "adds owner type info in its own attribute." $ do
      let cr l = ChunkRange l (Just (ChunkPoint (DataUID 3) 1)) (Just (ChunkPoint (DataUID 3) 2))
          vers = VDocVersion "<span data-uid=\"3\">asdf</span>"
          vers' l = VDocVersion $ "<span data-uid=\"3\">a<mark data-chunk-kind=\"" <> l <> "\" data-chunk-id=\"3\">s</mark>df</span>"

      chunkRangeMismatch (cr (ID 3 :: ID Note)) vers `shouldBe` []

      -- NOTE: if you change these, you will probably break css in the frontend.
      insertMarks [cr (ID 3 :: ID Note)]       vers `shouldBe` Right (vers' "note")
      insertMarks [cr (ID 3 :: ID Question)]   vers `shouldBe` Right (vers' "question")
      insertMarks [cr (ID 3 :: ID Discussion)] vers `shouldBe` Right (vers' "discussion")
      insertMarks [cr (ID 3 :: ID Edit)]       vers `shouldBe` Right (vers' "edit")

    it "crashes (assertion failed) if input is not canonicalized" $ do
      let eval :: Show e => Either e a -> IO a
          eval = either (throwIO . ErrorCall . show) pure

          bad :: Either VDocHTMLError (VDocVersion 'HTMLWithMarks)
          bad = insertMarks noChunkRanges $ VDocVersion "<wef>q"  -- (data-uid attr. missing in tag)

      eval bad `shouldThrow` anyException


  describe "chunkCanBeApplied" $ do
    let vers :: VDocVersion 'HTMLCanonical
        vers = VDocVersion "<span data-uid=\"1\">asdfasdf</span>"

        cr :: ChunkPoint -> ChunkRange Edit
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
        resolvePreTokens [PreMarkOpen "2" "whoof", PreMarkClose "2"]
          `shouldBe` Right []

      it "drops selections that have only tags in them, but no text" $ do
        resolvePreTokens [PreMarkOpen "2" "whoof", PreMarkOpen "8" "whoof", PreMarkClose "8", PreMarkClose "2"]
          `shouldBe` Right []

      it "renders marks as tags" $ do
        resolvePreTokens [PreMarkOpen "2" "whoof", PreToken $ ContentText "wef", PreMarkClose "2"]
          `shouldBe` Right [ TagOpen "mark" [Attr "data-chunk-id" "2", Attr "data-chunk-kind" "whoof"]
                           , ContentText "wef"
                           , TagClose "mark"
                           ]
        resolvePreTokens [ PreMarkOpen "2" "whoof"
                         , PreToken $ ContentText "wef"
                         , PreMarkOpen "8" "whoof"
                         , PreToken $ ContentText "puh"
                         , PreMarkClose "8"
                         , PreMarkClose "2"
                         ]
          `shouldBe`
               Right [ TagOpen "mark" [Attr "data-chunk-id" "2",Attr "data-chunk-kind" "whoof"]
                     , ContentText "wef"
                     , TagClose "mark"
                     , TagOpen "mark" [Attr "data-chunk-id" "8",Attr "data-chunk-kind" "whoof"]
                     , TagOpen "mark" [Attr "data-chunk-id" "2",Attr "data-chunk-kind" "whoof"]
                     , ContentText "puh"
                     , TagClose "mark"
                     , TagClose "mark"
                     ]

    context "with *in*consistent PreMarks" $ do
      it "fails" $ do
        let bad1 = [ PreMarkOpen "2" "whoof"
                   ]
            bad2 = [ PreMarkClose "8"
                   ]
        resolvePreTokens bad1
          `shouldBe` Left "resolvePreTokens: open without close: \
            \([PreMarkOpen \"2\" \"whoof\"]\
            \,ResolvePreTokensStack {_rptsOpen = fromList [(\"2\",\"whoof\")], _rptsWritten = [], _rptsReading = []})"
        resolvePreTokens bad2
          `shouldBe` Left "resolvePreTokens: close without open: \
            \([PreMarkClose \"8\"]\
            \,ResolvePreTokensStack {_rptsOpen = fromList [], _rptsWritten = [], _rptsReading = [PreMarkClose \"8\"]})"

  describe "preTokensToForest" $ do
    it "Correctly ignores broken tree structure of PreMarkOpen, PreMarkClose." $ do
      preTokensToForest [PreMarkOpen "1" "whoof"] `shouldBe` Right [Node (PreMarkOpen "1" "whoof") []]
      preTokensToForest [PreMarkClose "1"] `shouldBe` Right [Node (PreMarkClose "1") []]
