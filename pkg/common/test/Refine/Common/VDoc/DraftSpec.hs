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

module Refine.Common.VDoc.DraftSpec where

import Refine.Common.Prelude

import qualified Data.Set as Set
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as ST
import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck

import Refine.Common.Test.Arbitrary
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

  describe "selectionText" $ do
    let -- old implementation that has been refactored away.  we keep it to check if the new code
        -- still has the same meaning.
        selectionIsEmptyAlternative :: RawContent -> SelectionState -> Bool
        selectionIsEmptyAlternative (RawContent bs _) ss@(SelectionState _ s e) = s == e || multiLineCase
          where
            multiLineCase = case selectedBlocks ss (NEL.toList bs) of
              []        -> True
              [_]       -> assert (s /= e) False
              (b : bs') -> and [ ST.length (b ^. blockText) == (s ^. selectionOffset)
                               , e ^. selectionOffset == 0
                               , all (ST.null . view blockText) (init bs')
                               ]

    it "works" . property $ \(RawContentWithSelections rc sels) -> do
      (selectionIsEmpty rc <$> sels) `shouldBe` (selectionIsEmptyAlternative rc <$> sels)

  describe "mkSomeSegments" $ do
    it "works" $ do
      mkSomeSegments fst snd [((1, 3), 'o'), ((2, 4), 'x')]
        `shouldBe` [(1, Set.empty), (1, Set.fromList "o"), (2, Set.fromList "ox"), (2, Set.fromList "x")]

  describe "getMarkSelectors" $ do
    let cid0 = ContribIDNote (ID 13)
        cid1 = ContribIDNote (ID 35)
        block0 = BlockKey "0"
        block1 = BlockKey "1"

    it "works (no contribs)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = []
          want       = []
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning the entire block)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [(cid0, SelectionState False (SelectionPoint block0 0) (SelectionPoint block0 4))]
          want       = [(cid0, MarkSelector MarkSelectorTop block0 0, MarkSelector MarkSelectorBottom block0 0)]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning part of the block)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [(cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 4))]
          want       = [(cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 1)]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning the entire block, with an extra block style flying around)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890" & blockStyles .~ [((1, 2), Bold)]]
          marks      = [(cid0, SelectionState False (SelectionPoint block0 0) (SelectionPoint block0 4))]
          want       = [(cid0, MarkSelector MarkSelectorTop block0 0, MarkSelector MarkSelectorBottom block0 2)]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (one contrib in two parts)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 3))
                       , (cid0, SelectionState False (SelectionPoint block0 4) (SelectionPoint block0 7))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 1)
                       , (cid0, MarkSelector MarkSelectorTop block0 3, MarkSelector MarkSelectorBottom block0 3)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (one contrib in two parts spanning two blocks)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890", mkBlock "asdf"]
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block1 1))
                       , (cid0, SelectionState False (SelectionPoint block1 2) (SelectionPoint block1 3))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block1 0)
                       , (cid0, MarkSelector MarkSelectorTop block1 2, MarkSelector MarkSelectorBottom block1 2)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 4))
                       , (cid1, SelectionState True (SelectionPoint block0 3) (SelectionPoint block0 7))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 2)
                       , (cid1, MarkSelector MarkSelectorTop block0 2, MarkSelector MarkSelectorBottom block0 3)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs spanning two blocks)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890", mkBlock "asdf"]
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block1 3))
                       , (cid1, SelectionState True (SelectionPoint block1 1) (SelectionPoint block1 2))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block1 2)
                       , (cid1, MarkSelector MarkSelectorTop block1 1, MarkSelector MarkSelectorBottom block1 1)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs beginning in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 3))
                       , (cid1, SelectionState True (SelectionPoint block0 2) (SelectionPoint block0 4))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 1)
                       , (cid1, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 2)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs ending in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 3) (SelectionPoint block0 4))
                       , (cid1, SelectionState True (SelectionPoint block0 2) (SelectionPoint block0 4))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 2, MarkSelector MarkSelectorBottom block0 2)
                       , (cid1, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 2)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs beginning and ending in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 4))
                       , (cid1, SelectionState True (SelectionPoint block0 2) (SelectionPoint block0 4))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 1)
                       , (cid1, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 1)
                       ]
      getMarkSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (with entity)" $ do
      pendingWith "do they also have spans?  can we somehow distinguish them away in the css selector?"
