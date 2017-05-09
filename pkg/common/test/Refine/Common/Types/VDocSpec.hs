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

module Refine.Common.Types.VDocSpec where

import           Control.Exception (assert)
import           Control.Lens ((^.), (%~), view, _1, _2, _3)
import           Data.List as List
import           Data.Map as Map
import           Data.String.Conversions
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.Draft


rawContentToCompositeVDoc :: RawContentWithSelections -> CompositeVDoc
rawContentToCompositeVDoc (RawContentWithSelections rawContent selections)
    = assert (length selections == length es + length ns + length ds)
    $ CompositeVDoc un un un vers (fromList es) (fromList ns) (fromList ds)
  where
    un = undefined
    vers = rawContentToVDocVersion rawContent

    (es, ns, ds) = rotate ([], [], []) 0 selections

    rotate :: ([(ID Edit, Edit)], [(ID Note, Note)], [(ID Discussion, CompositeDiscussion)])
           -> Int
           -> [SelectionState]
           -> ([(ID Edit, Edit)], [(ID Note, Note)], [(ID Discussion, CompositeDiscussion)])
    rotate contribs _ []           = contribs
    rotate contribs i (sel : sels) = rotate (upd contribs) (i + 1) sels
      where
        upd = case i `mod` 3 of
          0 -> _1 %~ (build (Proxy :: Proxy Edit)       i (\r -> Edit un un r un un) sel :)
          1 -> _2 %~ (build (Proxy :: Proxy Note)       i (Note un un un) sel :)
          2 -> _3 %~ (build (Proxy :: Proxy Discussion) i (\r -> CompositeDiscussion (Discussion un un r) un) sel :)
          _ -> error "rawContentToCompositeVDoc: impossible."

    build :: Proxy a -> Int -> (ChunkRange -> b) -> SelectionState -> (ID a, b)
    build Proxy i cons sel = (ID $ fromIntegral i, cons $ selectionStateToChunkRange rawContent sel)


spec :: Spec
spec = do
  describe "rawContentFromCompositeVDoc" $ do
    it "does not crash" . property $ \rawContentWithSelections -> do
      let rawContent' = rawContentFromCompositeVDoc (rawContentToCompositeVDoc rawContentWithSelections)
      (length . show) rawContent' `shouldNotBe` 0

    describe "adds a plausible number of inline styles" $ do
      let checkNumRanges :: Maybe [[(EntityRange, Style)]] -> RawContentWithSelections -> Expectation
          checkNumRanges
              mExpected
              (RawContentWithSelections
                (rawContentBlocks %~ fmap deleteMarksFromBlock         -> rawContent)
                (nub . List.filter (not . selectionIsEmpty rawContent) -> selections)) = do

            let rawContent' = rawContentFromCompositeVDoc
                            $ rawContentToCompositeVDoc (RawContentWithSelections rawContent selections)

                numInlineStyles :: RawContent -> Int
                numInlineStyles = length . mconcat . inlineStyles

                inlineStyles :: RawContent -> [[(EntityRange, Style)]]
                inlineStyles = fmap (view blockStyles) . view rawContentBlocks

            numInlineStyles rawContent' `shouldSatisfy` (>= numInlineStyles rawContent + length selections)
            case mExpected of
              Just expected -> inlineStyles rawContent' `shouldBe` expected
              Nothing -> pure ()
                      -- traceM $ "you should have expected this: " <> show (inlineStyles rawContent')

      it "example 1" $ do
        let given = RawContentWithSelections
               (RawContent
                 [ Block ":" [] [] Header2 0 (Just (BlockKey "0"))
                 , Block "r" [] [] Header2 0 (Just (BlockKey "1"))
                 ]
                 mempty)
               [ SelectionState True (SelectionPoint (BlockKey "0") 0) (SelectionPoint (BlockKey "1") 0)
               , SelectionState False (SelectionPoint (BlockKey "0") 0) (SelectionPoint (BlockKey "1") 0)
               ]
            expected = [[((0,1),RangeEdit),((0,1),RangeComment)],[]]
        checkNumRanges (Just expected) given

      it "example 2" $ do
        let given = RawContentWithSelections
               (RawContent
                 [ Block "asdf_1234-#$!&" [] [] NormalText 0 (Just (BlockKey "0"))
                 ]
                 mempty)
               [ SelectionState False (SelectionPoint (BlockKey "0") 3) (SelectionPoint (BlockKey "0") 9)
               , SelectionState False (SelectionPoint (BlockKey "0") 6) (SelectionPoint (BlockKey "0") 12)
               ]
            expected = [[((3,6),RangeEdit),((6,6),RangeComment)]]
        checkNumRanges (Just expected) given

      it "(property)" . property $
        checkNumRanges Nothing


  describe "addMarksToBlocks" $ do
    describe "adds inline styles at the correct offsets" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef"]

          check :: EntityRange -> Spec
          check (i, j) = it (show (i, j)) $ do
            let marks = fromList [(3 :: ID Edit, SelectionState False p1 p2)]
                p1 = SelectionPoint (BlockKey "0") i
                p2 = SelectionPoint (BlockKey "0") j
            (view blockStyles <$> addMarksToBlocks marks (rawContent ^. rawContentBlocks))
              `shouldBe` [[((i, j - i), RangeEdit)]]

      check `mapM_` [ (i, j) | i <- [0..3], j <- [0..3], i < j ]

    describe "adds inline styles at the correct offsets accross neighboring blocks" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef", mkBlock "1234"]

          check :: EntityRange -> Spec
          check (i, j) = it (show (i, j)) $ do
            let marks = fromList [(3 :: ID Note, SelectionState False p1 p2)]
                p1 = SelectionPoint (BlockKey "0") i
                p2 = SelectionPoint (BlockKey "1") j
            (view blockStyles <$> addMarksToBlocks marks (rawContent ^. rawContentBlocks))
              `shouldBe` (List.filter (not . entityRangeIsEmpty . fst) <$>
                          [[((i, 3), RangeComment)], [((0, j), RangeComment)]])

      check `mapM_` [ (i, j) | i <- [0..3], j <- [0..4] ]

    describe "adds inline styles at the correct offsets accross distant blocks" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef", mkBlock "", mkBlock "...", mkBlock "*", mkBlock "1234", mkBlock "????"]

          check :: EntityRange -> Spec
          check (i, j) = it (show (i, j)) $ do
            let marks = fromList [(3 :: ID Note, SelectionState False p1 p2)]
                p1 = SelectionPoint (BlockKey "0") i
                p2 = SelectionPoint (BlockKey "4") j
            (view blockStyles <$> addMarksToBlocks marks (rawContent ^. rawContentBlocks))
              `shouldBe` (List.filter (not . entityRangeIsEmpty . fst) <$>
                          [[((i, 3), RangeComment)], [], [((0, 3), RangeComment)], [((0, 1), RangeComment)], [((0, j), RangeComment)], []])

      check `mapM_` [ (i, j) | i <- [0..3], j <- [0..4] ]

    it "is idempotent (together with 'deleteMarksFromBlock')" . property $
      \(RawContentWithSelections rawContent (fromList . zip [(0 :: ID Edit)..] -> selections)) -> do
        let blocks   = rawContent ^. rawContentBlocks
            blocks'  = addMarksToBlocks selections (deleteMarksFromBlock <$> blocks)
            blocks'' = addMarksToBlocks selections (deleteMarksFromBlock <$> blocks')
        blocks' `shouldBe` blocks''

  describe "selectionIsEmpty" $ do
    it "works" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef", mkBlock "", mkBlock "...", mkBlock "*", mkBlock "1234", mkBlock "????"]
          mksel :: Int -> Int -> Int -> Int -> SelectionState
          mksel sk so ek eo = SelectionState False
            (SelectionPoint (BlockKey . cs . show $ sk) so)
            (SelectionPoint (BlockKey . cs . show $ ek) eo)

      mksel 0 0 0 0 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 0 2 0 2 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 0 3 0 3 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 0 3 1 0 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 1 0 1 0 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 1 0 2 0 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 0 3 2 0 `shouldSatisfy`    selectionIsEmpty rawContent
      mksel 0 0 0 1 `shouldNotSatisfy` selectionIsEmpty rawContent
      mksel 0 2 2 0 `shouldNotSatisfy` selectionIsEmpty rawContent
      mksel 0 3 2 1 `shouldNotSatisfy` selectionIsEmpty rawContent
      mksel 2 3 5 0 `shouldNotSatisfy` selectionIsEmpty rawContent
