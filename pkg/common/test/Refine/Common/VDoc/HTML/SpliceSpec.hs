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

import           Control.Exception (evaluate)
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


noChunkRanges :: [ChunkRange ()]
noChunkRanges = []


spec :: Spec
spec = parallel $ do
  describe "insertMarks" $ do
    it "does not change version if chunk list is empty." . property . forAll arbitraryCanonicalVDocVersion $ do
      \vers -> do
        _unVDocVersion (insertMarks noChunkRanges vers) `shouldBe` _unVDocVersion vers

    it "self-closing tags with closing `/`." $ do
      let vers = vdocVersionFromST "<div data-uid=\"1\"><br data-uid=\"2\"/>el</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 0))
                                                   (Just (ChunkPoint (DataUID 1) 1))
      chunkRangeErrors r vers `shouldBe` []
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "fails on self-closing tags without closing `/` (should be resolved by canonicalization)." $ do
      pendingWith "#16"
      let vers = vdocVersionFromST "<div data-uid=\"1\"><br>el</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 0))
                                                   (Just (ChunkPoint (DataUID 1) 1))
      (evaluate . length . show $ chunkRangeErrors r vers) `shouldThrow` anyException

    it "regression (1)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span><div O=\"\" data-uid=\"2\"></div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 3))
                                                   (Just (ChunkPoint (DataUID 2) 0))
      chunkRangeErrors r vers `shouldBe` []
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "regression (2)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span><div O=\"\" data-uid=\"2\">.</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 3))
                                                   (Just (ChunkPoint (DataUID 2) 0))
      chunkRangeErrors r vers `shouldBe` []
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "regression (3)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span>"
          r :: ChunkRange Edit = ChunkRange (ID 3) (Just (ChunkPoint (DataUID 1) 2)) Nothing
      chunkRangeErrors r vers `shouldBe` []
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "regression (4)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span>"
          rs :: [ChunkRange Edit] = [ ChunkRange (ID 3) (Just (ChunkPoint (DataUID 1) 2)) Nothing
                                    , ChunkRange (ID 4) (Just (ChunkPoint (DataUID 1) 0)) Nothing
                                    ]
      insertMarks rs vers `shouldNotBe` VDocVersion []

    it "regression (5)." $ do
      let vers = vdocVersionFromST $
            "<span data-uid=\"4\">zC9E</span>" <>
            "<n data-uid=\"5\"><f data-uid=\"6\"></f>;</n>" <>
            "<T data-uid=\"7\"></T><i data-uid=\"8\"></i>"
          rs :: [ChunkRange Edit]
          rs = [ ChunkRange (ID 0) (Just (ChunkPoint (DataUID 4) 2)) (Just (ChunkPoint (DataUID 6) 0))
               , ChunkRange (ID 2) Nothing                           (Just (ChunkPoint (DataUID 5) 0))
               ]

      insertMarks rs vers `shouldNotBe` VDocVersion []

    it "regression (6)." $ do
      let vers = vdocVersionFromST $
            "<span data-uid=\"4\">zC9E</span>" <>
            "<n data-uid=\"5\"><f data-uid=\"6\">zz</f>;</n>" <>
            "<T data-uid=\"7\"></T><i data-uid=\"8\"></i>"

          -- chunk ranges must be non-empty.
          empty :: ChunkRange Edit
          empty = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 4) 4)) (Just (ChunkPoint (DataUID 5) 0))

          -- datauid must point to direct parent of text node.
          nonleaf :: ChunkRange Edit
          nonleaf = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 5) 1)) (Just (ChunkPoint (DataUID 5) 2))

      pendingWith "TODO"
      chunkRangeErrors empty   vers `shouldNotBe` []
      chunkRangeErrors nonleaf vers `shouldNotBe` []

    it "generates valid output on arbitrary valid chunkranges." . property $ do
      \(VersWithRanges vers rs) -> do
        pendingWith "test data generation is broken."
        insertMarks rs vers `shouldNotBe` VDocVersion []

    it "marks are inserted under the correct parent node." $ do
      pending

    it "adds owner type info in its own attribute." $ do
      let cr l = ChunkRange l (Just (ChunkPoint (DataUID 3) 1)) (Just (ChunkPoint (DataUID 3) 2))
          vers = vdocVersionFromST "<span data-uid=\"3\">asdf</span>"
          vers' l = vdocVersionFromST $
            "<span data-uid=\"1\">a<mark data-chunk-kind=\"" <> l <>
            "\" data-chunk-id=\"3\" data-uid=\"2\">s</mark>df</span>"

      chunkRangeErrors (cr (ID 3 :: ID Note)) vers `shouldBe` []

      -- NOTE: if you change these, you will probably break css in the frontend.
      insertMarks [cr (ID 3 :: ID Note)]       vers `shouldBe` vers' "note"
      insertMarks [cr (ID 3 :: ID Question)]   vers `shouldBe` vers' "question"
      insertMarks [cr (ID 3 :: ID Discussion)] vers `shouldBe` vers' "discussion"
      insertMarks [cr (ID 3 :: ID Edit)]       vers `shouldBe` vers' "edit"


  describe "chunkCanBeApplied" $ do
    let vers :: VDocVersion 'HTMLCanonical
        vers = vdocVersionFromST "<span data-uid=\"1\">asdfasdf</span>"

        cr :: ChunkPoint -> ChunkRange Edit
        cr p = ChunkRange (ID 3) (Just p) Nothing

        good = ChunkPoint (DataUID 1) 3
        bad1 = ChunkPoint (DataUID 1) 189
        bad2 = ChunkPoint (DataUID 4) 3


    it "returns True on valid chunks." $ do
      chunkRangeErrors (cr good) vers `shouldBe` []

    it "returns False on out-of-bounds offset." $ do
      chunkRangeErrors (cr bad1) vers `shouldNotBe` []

    it "returns False on invalid data-uid." $ do
      chunkRangeErrors (cr bad2) vers `shouldNotBe` []


  describe "resolvePreTokens" $ do
    let runPreTokenForest :: Forest PreToken -> [Token]
        runPreTokenForest = tokensFromForest . fmap (fmap runPreToken)

    context "w/o PreMarks" $ do
      it "inverts enablePreTokens" . property . forAll arbitraryCanonicalVDocVersion $ do
        \(VDocVersion forest) -> do
          let go   :: [Token] = runPreTokenForest $ enablePreTokens forest
              stay :: [Token] = tokensFromForest forest
          go `shouldBe` stay

    context "with consistent PreMarks" $ do
      it "removes empty selections" $ do
        resolvePreTokens [PreMarkOpen "2" "whoof", PreMarkClose "2"]
          `shouldBe` []

      it "drops selections that have only tags in them, but no text" $ do
        resolvePreTokens [PreMarkOpen "2" "whoof", PreMarkOpen "8" "whoof", PreMarkClose "8", PreMarkClose "2"]
          `shouldBe` []

      it "renders marks as tags" $ do
        resolvePreTokens [PreMarkOpen "2" "whoof", PreToken $ ContentText "wef", PreMarkClose "2"]
          `shouldBe` [ TagOpen "mark" [Attr "data-chunk-id" "2", Attr "data-chunk-kind" "whoof"]
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
          `shouldBe` [ TagOpen "mark" [Attr "data-chunk-id" "2",Attr "data-chunk-kind" "whoof"]
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
        pendingWith "#16"

        let bad1 = [ PreMarkOpen "2" "whoof"
                   ]
            bad2 = [ PreMarkClose "8"
                   ]
        evaluate (resolvePreTokens bad1) `shouldThrow` anyException
        evaluate (resolvePreTokens bad2) `shouldThrow` anyException
