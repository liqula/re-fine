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
import           Control.Lens (has, (^.))
import           Data.List (nub, sort, foldl')
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.String.Conversions ((<>))
import qualified Data.Text as ST
import           Data.Tree
import           Data.Void
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser
import           Text.HTML.Tree
import           Web.HttpApiData

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.CanonicalizeSpec (shouldBeVDocVersion)
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Enhance (addUIInfoToForest)
import Refine.Common.VDoc.HTML.Splice


noChunkRanges :: [ChunkRange ()]
noChunkRanges = []


spec :: Spec
spec = parallel $ do

  -- * createChunkRangeErrors

  describe "createChunkRangeErrors" $ do

    describe "tests for test data generation (software is fractal)." $ do
      it "works (1)." . property $
        \(vers :: VDocVersion 'HTMLCanonical) ->
          let ps = allChunkPoints vers in ps `shouldBe` nub ps

      it "works (2)." . property $
        \(vers :: VDocVersion 'HTMLCanonical) ->
          let ps = allChunkPoints vers in (snd <$> ps) `shouldBe` nub (snd <$> ps)

      it "works (3)." . property $
        \(vers :: VDocVersion 'HTMLCanonical) ->
          let ps = allChunkPoints vers in (fst <$> ps) `shouldBe` sort (fst <$> ps)

    it "generates valid output on arbitrary valid chunkranges." . property $ do
      \(VersWithRanges vers rs) -> do
        (\r -> chunkRangeErrors r vers `shouldBe` []) `mapM_` rs

    let vers = vdocVersionFromST $
          "<span data-uid=\"4\">zC9E</span>" <>
          "<n data-uid=\"5\"><f data-uid=\"6\">zz</f>;</n>" <>
          "<T data-uid=\"7\"></T><i data-uid=\"8\"></i>"

    it "works (1)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 100) 4)) (Just (ChunkPoint (DataUID 30) 0)))
        vers `shouldSatisfy` any (has _ChunkRangeBadDataUID)

    it "works (2)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 5) 1000)) (Just (ChunkPoint (DataUID 5) 2000)))
        vers `shouldSatisfy` any (has _ChunkRangeOffsetOutOfBounds)

    it "works (3)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 5) 1)) (Just (ChunkPoint (DataUID 5) 2)))
        vers `shouldBe` []

    it "works (4)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 4) 4)) (Just (ChunkPoint (DataUID 5) 0)))
        vers `shouldSatisfy` any (has _ChunkRangeEmpty)

    it "works (5)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 4) 2)) (Just (ChunkPoint (DataUID 4) 2)))
        vers `shouldSatisfy` any (has _ChunkRangeEmpty)

    it "works (6)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 4) 2)) (Just (ChunkPoint (DataUID 4) 3)))
        vers `shouldBe` []

    it "works (7)." $ do
      createChunkRangeErrors (CreateChunkRange (Just (ChunkPoint (DataUID 4) 2)) Nothing)
        vers `shouldBe` []

    it "works (8)." $ do
      createChunkRangeErrors (CreateChunkRange Nothing (Just (ChunkPoint (DataUID 4) 2)))
        vers `shouldBe` []

    it "works (9)." $ do
      createChunkRangeErrors (CreateChunkRange Nothing (Just (ChunkPoint (DataUID 5) 0)))
        vers `shouldBe` []

    it "works (10)." $ do
      let vers' = VDocVersion
            [ Node (TagOpen "span" [Attr "data-uid" "183949"]) [Node {rootLabel = ContentText "wef", subForest = []}]
            , Node {rootLabel = TagOpen "phoo" [Attr "data-uid" "50"], subForest =
                [ Node {rootLabel = TagOpen "wef" [Attr "data-uid" "52"], subForest =
                    [ Node {rootLabel = ContentText "**", subForest = []}
                    ]}
                , Node {rootLabel = ContentText "wef", subForest = []}
                ]}
            ]


          rs = allNonEmptyCreateChunkRanges_ vers'
          check (_, r) = do
            createChunkRangeErrors r vers' `shouldNotSatisfy` any (has _ChunkRangeBadDataUID)
            createChunkRangeErrors r vers' `shouldNotSatisfy` any (has _ChunkRangeOffsetOutOfBounds)
            createChunkRangeErrors r vers' `shouldNotSatisfy` any (has _ChunkRangeEmpty)
      check `mapM_` rs


  -- * splitAtOffset

  describe "splitAtOffset" $ do
    it "maintains tree structure if nothing is inserted." $ do
      let VDocVersion (enablePreTokens -> forest) = vdocVersionFromST "<n><f></f>abc</n>"
          show_   = renderTokens . tokensFromForest . fmap (fmap runPreToken)
          nothing = []

      show_ (splitAtOffset 0 nothing forest) `shouldBe` "<n><f></f>abc</n>"
      show_ (splitAtOffset 1 nothing forest) `shouldBe` "<n><f></f>abc</n>"
      show_ (splitAtOffset 2 nothing forest) `shouldBe` "<n><f></f>abc</n>"
      show_ (splitAtOffset 3 nothing forest) `shouldBe` "<n><f></f>abc</n>"

    it "inserts tokens at the given offset." $ do
      let VDocVersion (enablePreTokens -> forest) = vdocVersionFromST "<n><f></f>abc</n>"
          show_   = renderTokens . tokensFromForest . fmap (fmap runPreToken)
          star    = [Node (PreToken (ContentText "*")) []]

      show_ (splitAtOffset 0 star    forest) `shouldBe` "<n><f></f>*abc</n>"
      show_ (splitAtOffset 1 star    forest) `shouldBe` "<n><f></f>a*bc</n>"
      show_ (splitAtOffset 2 star    forest) `shouldBe` "<n><f></f>ab*c</n>"
      show_ (splitAtOffset 3 star    forest) `shouldBe` "<n><f></f>abc*</n>"


  -- * insertMarks

  describe "insertMarks" $ do
    it "does not change version if chunk list is empty." . property $ do
      \(vers :: VDocVersion 'HTMLCanonical) -> do
        insertMarks noChunkRanges vers ^. unVDocVersion `shouldBe` addUIInfoToForest (vers ^. unVDocVersion)

    it "generates valid output on arbitrary valid chunkranges." . property $ do
      \(VersWithRanges vers rs) -> do
        insertMarks rs vers `shouldNotBe` VDocVersion []

    it "generates valid output on arbitrary valid chunkranges (incrementally)." . property $ do
      \(VersWithRanges vers (r : rs)) -> do
        let incremental = foldl' (\vers' r' -> insertMoreMarks [r'] vers') (insertMarks [r] vers) rs
            atonce = insertMarks (r : rs) vers
        incremental `shouldBeLikeVDocVersion` atonce

    it "marks are inserted under the correct parent node." $ do
      pendingWith "not sure how to implement this test."

    it "handles self-closing tags with closing `/`." $ do
      let vers = vdocVersionFromST "<div data-uid=\"1\"><br data-uid=\"2\"/>el</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 0))
                                                   (Just (ChunkPoint (DataUID 1) 1))
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "adds owner type info in its own attribute." $ do
      let cr l = ChunkRange l (Just (ChunkPoint (DataUID 3) 1)) (Just (ChunkPoint (DataUID 3) 2))
          vers = vdocVersionFromST "<span data-uid=\"3\">asdf</span>"
          vers' l = VDocVersion . addUIInfoToForest . _unVDocVersion . vdocVersionFromST $
            "<span data-uid=\"1\">a<mark data-contribution-kind=\"" <> l <> "\" data-contribution-id=\"3\">s</mark>df</span>"

      chunkRangeErrors (cr (ID 3 :: ID Note)) vers `shouldBe` []

      -- NOTE: if you change these, you will probably break css in the frontend.
      insertMarks [cr (ID 3 :: ID Note)]       vers `shouldBe` vers' "note"
      insertMarks [cr (ID 3 :: ID Question)]   vers `shouldBe` vers' "question"
      insertMarks [cr (ID 3 :: ID Discussion)] vers `shouldBe` vers' "discussion"
      insertMarks [cr (ID 3 :: ID Edit)]       vers `shouldBe` vers' "edit"

    it "mark tag children never contain other tags."
      pending

    it "regression (1)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span><div O=\"\" data-uid=\"2\"></div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 3))
                                                   (Just (ChunkPoint (DataUID 2) 0))
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "regression (2)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span><div O=\"\" data-uid=\"2\">.</div>"
          r :: ChunkRange Edit = ChunkRange (ID 1) (Just (ChunkPoint (DataUID 1) 3))
                                                   (Just (ChunkPoint (DataUID 2) 0))
      insertMarks [r] vers `shouldNotBe` VDocVersion []

    it "regression (3)." $ do
      let vers = vdocVersionFromST "<span data-uid=\"1\">whee</span>"
          r :: ChunkRange Edit = ChunkRange (ID 3) (Just (ChunkPoint (DataUID 1) 2)) Nothing
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
          r1, r2 :: ChunkRange Edit
          r1 = ChunkRange (ID 0) (Just (ChunkPoint (DataUID 4) 2)) (Just (ChunkPoint (DataUID 6) 0))
          r2 = ChunkRange (ID 2) Nothing                           (Just (ChunkPoint (DataUID 5) 0))
      insertMarks [r1, r2] vers `shouldNotBe` VDocVersion []

    it "regression (6)." $ do
      let vers = VDocVersion [Node (TagOpen "span" [Attr "data-uid" "61"]) [Node (ContentText "g") []]]

          r1, r2 :: ChunkRange Edit
          r1 = ChunkRange (ID 1) Nothing Nothing
          r2 = ChunkRange (ID 2) Nothing Nothing

      chunkRangeErrors r1 vers `shouldBe` []
      chunkRangeErrors r2 vers `shouldBe` []

      insertMarks [r1] vers `shouldNotBe` VDocVersion []
      insertMarks [r2] vers `shouldNotBe` VDocVersion []
      insertMarks [r1, r2] vers `shouldNotBe` VDocVersion []

    it "regression (7)." $ do
      let vers = VDocVersion [Node {rootLabel = TagOpen "phoo" [Attr "data-uid" "7"], subForest = [Node {rootLabel = ContentText "C", subForest = []},Node {rootLabel = TagSelfClose "hr" [Attr "data-uid" "8"], subForest = []},Node {rootLabel = TagOpen "phoo" [Attr "data-uid" "9"], subForest = []},Node {rootLabel = TagOpen "phoo" [Attr "data-uid" "10"], subForest = [Node {rootLabel = ContentText "wef", subForest = []}]},Node {rootLabel = TagOpen "x123" [Attr "data-uid" "11"], subForest = [Node {rootLabel = TagSelfClose "wef" [Attr "data-uid" "12"], subForest = []}]}]}]

          rs :: [ChunkRange Edit]
          rs = [ChunkRange (ID 3) (Just (ChunkPoint (DataUID 7) 1)) Nothing]

      (\r -> chunkRangeErrors r vers `shouldBe` []) `mapM_` rs
      insertMarks rs vers `shouldNotBe` VDocVersion []

    it "regression (8)." $ do
      let (VersWithRanges vers [r]) = VersWithRanges (VDocVersion [Node {rootLabel = TagOpen "x123" [Attr "data-uid" "12"], subForest = [Node {rootLabel = TagOpen "wef" [Attr "data-uid" "13"], subForest = [Node {rootLabel = ContentText "phoo", subForest = []}]},Node {rootLabel = TagSelfClose "br" [Attr "data-uid" "14"], subForest = []},Node {rootLabel = TagSelfClose "phoo" [Attr "data-uid" "15"], subForest = []},Node {rootLabel = ContentText ";", subForest = []},Node {rootLabel = TagSelfClose "hr" [Attr "data-uid" "16"], subForest = []}]}]) [ChunkRange {_chunkRangeLabel = ID {_unID = 209}, _chunkRangeBegin = Just ChunkPoint {_chunkPointNode = 12, _chunkPointOffset = 0}, _chunkRangeEnd = Nothing}]
      chunkRangeErrors r vers `shouldBe` []
      insertMarks [r] vers `shouldNotBe` VDocVersion []


  -- * resolvePreTokens

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
        resolvePreTokens [PreMarkOpen (ID 2) ContribKindNote, PreMarkClose (ID 2)]
          `shouldBe` []

      it "drops selections that have only tags in them, but no text" $ do
        resolvePreTokens [ PreMarkOpen (ID 2) ContribKindNote
                         , PreMarkOpen (ID 8) ContribKindNote
                         , PreMarkClose (ID 8)
                         , PreMarkClose (ID 2)
                         ]
          `shouldBe` []

      it "renders marks as tags" $ do
        resolvePreTokens [PreMarkOpen (ID 2) ContribKindNote, PreToken $ ContentText "wef", PreMarkClose (ID 2)]
          `shouldBe` [ TagOpen "mark" [Attr "data-contribution-id" "2", Attr "data-contribution-kind" "note"]
                     , ContentText "wef"
                     , TagClose "mark"
                     ]
        resolvePreTokens [ PreMarkOpen (ID 2) ContribKindNote
                         , PreToken $ ContentText "wef"
                         , PreMarkOpen (ID 8) ContribKindNote
                         , PreToken $ ContentText "puh"
                         , PreMarkClose (ID 8)
                         , PreMarkClose (ID 2)
                         ]
          `shouldBe` [ TagOpen "mark" [Attr "data-contribution-id" "2", Attr "data-contribution-kind" "note"]
                     , ContentText "wef"
                     , TagClose "mark"
                     , TagOpen "mark" [Attr "data-contribution-id" "8", Attr "data-contribution-kind" "note"]
                     , TagOpen "mark" [Attr "data-contribution-id" "2", Attr "data-contribution-kind" "note"]
                     , ContentText "puh"
                     , TagClose "mark"
                     , TagClose "mark"
                     ]

    context "with *in*consistent PreMarks" $ do
      it "fails" $ do
        pendingWith "#16"

        let bad1 = [ PreMarkOpen (ID 2) ContribKindNote
                   ]
            bad2 = [ PreMarkClose (ID 8)
                   ]
        evaluate (resolvePreTokens bad1) `shouldThrow` anyException
        evaluate (resolvePreTokens bad2) `shouldThrow` anyException


-- * helpers

-- | The order in which 'ChunkRange's are inserted changes the resulting token forest.
-- This function can be used to generate values from 'VDocVersions' that can be tested for equality
-- to abstract from these differences.  It returns a list of pairs of 'DataChunkID' set and number
-- of characters that follow in the 'VDocVersion' covered by those chunk ids.
marksEquivalenceClass :: VDocVersion 'HTMLWithMarks -> [(Set (ID Void), Int)]
marksEquivalenceClass (VDocVersion forest) = dfs mempty forest
  where
    push (Attr "data-contribution-id" (parseUrlPiece -> Right v) : _) opens = Set.insert v opens
    push (_ : as) opens = push as opens
    push [] _ = error "marksEquivalenceClass: mark tag without data-contribution-id attribute!"

    dfs :: Set (ID Void) -> Forest Token -> [(Set (ID Void), Int)]
    dfs opens (Node (TagOpen "mark" attrs) children : siblings) =
      dfs (push attrs opens) children <> dfs opens siblings

    dfs opens (Node (ContentText s) [] : siblings) =
      (opens, ST.length s) : dfs opens siblings

    dfs opens (Node _ children : siblings) =
      dfs opens children <> dfs opens siblings

    dfs _ [] = []

-- | Call 'shouldBeVDocVersion' if 'marksEquivalenceClass' values do not match.
shouldBeLikeVDocVersion :: VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLWithMarks -> Expectation
shouldBeLikeVDocVersion vers vers' =
  if marksEquivalenceClass vers == marksEquivalenceClass vers'
    then pure ()
    else vers `shouldBeVDocVersion` vers'
