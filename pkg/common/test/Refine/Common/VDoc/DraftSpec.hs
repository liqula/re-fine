{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Refine.Common.VDoc.DraftSpec where

import Refine.Common.Prelude

import qualified Data.Set as Set
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck

import Refine.Common.Test.Arbitrary ()
import Refine.Common.Test.Samples ()  -- (just importing it so we know it compiles.)
import Refine.Common.Types
import Refine.Common.VDoc.Draft


spec :: Spec
spec = do
  describe "RawContent" $ do
    roundtripSpecs (Proxy @RawContent)

  describe "rawContentToVDocVersion, rawContentFromVDocVersion" $ do
    it "rawContentFromVDocVersion . rawContentToVDocVersion == id" . property $ \(rawContent :: RawContent) -> do
      rawContentFromVDocVersion (rawContentToVDocVersion rawContent) `shouldBe` rawContent

  describe "separateStyles, joinStyles" $ do
    it "joinStyles . separateStyles == id" . property $ \(rawContent :: RawContent) -> do
      joinStyles (separateStyles rawContent) `shouldBe` rawContent
    it "separateStyles . joinStyles . separateStyles == separateStyles" . property $ \(rawContent :: RawContent) -> do
      let ss = separateStyles rawContent
      separateStyles (joinStyles ss) `shouldBe` ss

  describe "mkSomeSegments" $ do
    it "works" $ do
      mkSomeSegments fst snd [(EntityRange 1 3, 'o'), (EntityRange 2 4, 'x')]
        `shouldBe` [(1, Set.empty), (1, Set.fromList "o"), (2, Set.fromList "ox"), (2, Set.fromList "x")]

  describe "getLeafSelectors" $ do
    let cid0 = ContribIDNote (ID 13)
        cid1 = ContribIDNote (ID 35)
        block0 = BlockIndex 0 $ BlockKey "0"
        block1 = BlockIndex 1 $ BlockKey "1"

    it "works (no contribs)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = []
          want       = []
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning the entire block)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [(cid0, Range (Position block0 0) (Position block0 4))]
          want       = [(cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 0), Position (block0 ^. blockIndexKey) (SpanIndex 0 0))]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning part of the block)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [(cid0, Range (Position block0 2) (Position block0 4))]
          want       = [(cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 1))]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning the entire block, with an extra block style flying around)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890" & blockStyles .~ [(EntityRange 1 2, Bold)]]
          marks      = [(cid0, Range (Position block0 0) (Position block0 4))]
          want       = [(cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 0), Position (block0 ^. blockIndexKey) (SpanIndex 0 2))]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (one contrib in two parts)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 3))
                       , (cid0, Range (Position block0 4) (Position block0 7))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 1))
                       , (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 3), Position (block0 ^. blockIndexKey) (SpanIndex 0 3))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (one contrib in two parts spanning two blocks)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890", mkBlock "asdf"]
          marks      = [ (cid0, Range (Position block0 2) (Position block1 1))
                       , (cid0, Range (Position block1 2) (Position block1 3))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block1 ^. blockIndexKey) (SpanIndex 0 0))
                       , (cid0, Position (block1 ^. blockIndexKey) (SpanIndex 0 2), Position (block1 ^. blockIndexKey) (SpanIndex 0 2))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 4))
                       , (cid1, Range (Position block0 3) (Position block0 7))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 2))
                       , (cid1, Position (block0 ^. blockIndexKey) (SpanIndex 0 2), Position (block0 ^. blockIndexKey) (SpanIndex 0 3))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs spanning two blocks)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890", mkBlock "asdf"]
          marks      = [ (cid0, Range (Position block0 2) (Position block1 3))
                       , (cid1, Range (Position block1 1) (Position block1 2))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block1 ^. blockIndexKey) (SpanIndex 0 2))
                       , (cid1, Position (block1 ^. blockIndexKey) (SpanIndex 0 1), Position (block1 ^. blockIndexKey) (SpanIndex 0 1))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs beginning in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 3))
                       , (cid1, Range (Position block0 2) (Position block0 4))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 1))
                       , (cid1, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 2))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs ending in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 3) (Position block0 4))
                       , (cid1, Range (Position block0 2) (Position block0 4))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 2), Position (block0 ^. blockIndexKey) (SpanIndex 0 2))
                       , (cid1, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 2))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs beginning and ending in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 4))
                       , (cid1, Range (Position block0 2) (Position block0 4))
                       ]
          want       = [ (cid0, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 1))
                       , (cid1, Position (block0 ^. blockIndexKey) (SpanIndex 0 1), Position (block0 ^. blockIndexKey) (SpanIndex 0 1))
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (with entity)" $ do
      pendingWith "do they also have spans?  can we somehow distinguish them away in the css selector?"
