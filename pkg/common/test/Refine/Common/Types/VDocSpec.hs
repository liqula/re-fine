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

import Refine.Common.Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.Draft

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

rawContentToCompositeVDoc :: RawContentWithSelections -> CompositeVDoc
rawContentToCompositeVDoc (RawContentWithSelections rawContent selections)
    = assert (length selections == length es + length ns + length ds)
    $ CompositeVDoc un un vers (Map.fromList es) (Map.fromList ns) (Map.fromList ds)
  where
    un = assert False undefined
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
          0 -> _1 %~ (build (Proxy :: Proxy Edit)       i (\r -> Edit un un r un un un) sel :)
          1 -> _2 %~ (build (Proxy :: Proxy Note)       i (Note un un un) sel :)
          2 -> _3 %~ (build (Proxy :: Proxy Discussion) i (\r -> CompositeDiscussion (Discussion un un r) un) sel :)
          _ -> error "rawContentToCompositeVDoc: impossible."

    build :: Proxy a -> Int -> (ChunkRange -> b) -> SelectionState -> (ID a, b)
    build Proxy i cons sel = (ID $ fromIntegral i, cons $ selectionStateToChunkRange rawContent sel)


mark1 :: Style
mark1 = Mark (ContribIDEdit (ID 0))

mark2 :: Style
mark2 = Mark (ContribIDNote (ID 1))


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
                (deleteMarksFromRawContentIf (const True)              -> rawContent)
                (nub . List.filter (not . selectionIsEmpty rawContent) -> selections)) = do

            let rawContent' = rawContentFromCompositeVDoc
                            $ rawContentToCompositeVDoc (RawContentWithSelections rawContent selections)

                numInlineStyles :: RawContent -> Int
                numInlineStyles = length . mconcat . inlineStyles

                inlineStyles :: RawContent -> [[(EntityRange, Style)]]
                inlineStyles = fmap (view blockStyles) . view rawContentBlocks

            numInlineStyles rawContent' `shouldSatisfy` (>= numInlineStyles rawContent + length selections)
            let sanitize = (sort <$>) . (fst <$$>)
            case mExpected of
              Just expected -> sanitize (inlineStyles rawContent') `shouldBe` sanitize expected
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
            expected = [[((0, 1), mark1), ((0, 1), mark2)], []]
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
            expected = [[((3, 6), mark1), ((6, 6), mark2)]]
        checkNumRanges (Just expected) given

      it "(property)" . property $
        checkNumRanges Nothing


  describe "addMarksToBlocks" $ do
    describe "adds inline styles at the correct offsets" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef"]

          test :: EntityRange -> Spec
          test (i, j) = it (show (i, j)) $ do
            let marks = [(ContribIDEdit 3, SelectionState False p1 p2)]
                p1 = SelectionPoint (BlockKey "0") i
                p2 = SelectionPoint (BlockKey "0") j

                have :: [[EntityRange]]
                have = fst <$$> view blockStyles <$> addMarksToBlocks marks (rawContent ^. rawContentBlocks)

                want :: [[EntityRange]]
                want = [[(i, j - i)]]

            have `shouldBe` List.filter (not . entityRangeIsEmpty) <$> want

      test `mapM_` [ (i, j) | i <- [0..3], j <- [0..3], i < j ]

    describe "adds inline styles at the correct offsets accross neighboring blocks" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef", mkBlock "1234"]

          test :: EntityRange -> Spec
          test (i, j) = it (show (i, j)) $ do
            let marks = [(ContribIDNote 3, SelectionState False p1 p2)]
                p1 = SelectionPoint (BlockKey "0") i
                p2 = SelectionPoint (BlockKey "1") j

                have :: [[EntityRange]]
                have = fst <$$> view blockStyles <$> addMarksToBlocks marks (rawContent ^. rawContentBlocks)

                want :: [[EntityRange]]
                want = [[(i, 3)], [(0, j)]]

            have `shouldBe` List.filter (not . entityRangeIsEmpty) <$> want

      test `mapM_` [ (i, j) | i <- [0..3], j <- [0..4] ]

    describe "adds inline styles at the correct offsets accross distant blocks" $ do
      let rawContent = initBlockKeys $ mkRawContent [mkBlock "wef", mkBlock "", mkBlock "...", mkBlock "*", mkBlock "1234", mkBlock "????"]

          test :: EntityRange -> Spec
          test (i, j) = it (show (i, j)) $ do
            let marks = [(ContribIDNote 3, SelectionState False p1 p2)]
                p1 = SelectionPoint (BlockKey "0") i
                p2 = SelectionPoint (BlockKey "4") j

                have :: [[EntityRange]]
                have = fst <$$> view blockStyles <$> addMarksToBlocks marks (rawContent ^. rawContentBlocks)

                want :: [[EntityRange]]
                want = [[(i, 3)], [], [(0, 3)], [(0, 1)], [(0, j)], []]

            have `shouldBe` List.filter (not . entityRangeIsEmpty) <$> want

      test `mapM_` [ (i, j) | i <- [0..3], j <- [0..4] ]

    it "works when ranges start and/or end on the same point" $ do
      let cid0 = ContribIDNote (ID 13)
          cid1 = ContribIDNote (ID 35)
          block0 = BlockKey "0"

          rawContent = initBlockKeys $ mkRawContent [mkBlock "1234567890"]
          marks s1 e1 s2 e2 = [ (cid0, SelectionState False (SelectionPoint block0 s1) (SelectionPoint block0 e1))
                              , (cid1, SelectionState False (SelectionPoint block0 s2) (SelectionPoint block0 e2))
                              ]
          rawContent' = RawContent mempty mempty
      addMarksToRawContent (marks 3 4 2 4) rawContent `shouldNotBe` rawContent'
      addMarksToRawContent (marks 2 4 3 4) rawContent `shouldNotBe` rawContent'
      addMarksToRawContent (marks 2 4 2 4) rawContent `shouldNotBe` rawContent'

    it "is idempotent (together with 'deleteMarksFromBlock')" . property $
      \(RawContentWithSelections rawContent (zip (ContribIDEdit <$> [0..]) -> selections)) -> do
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
