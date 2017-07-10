{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- pattern completeness checker has problems with pattern synonyms

module Refine.Common.VDoc.OTSpec where

import Refine.Common.Prelude

import           Data.List (groupBy)
import           Data.Char
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import           Test.QuickCheck
import           Test.Hspec

import Refine.Common.OT
import Refine.Common.OTSpec hiding (spec)
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types.Core hiding (Edit)
import Refine.Common.VDoc.OT

{-# ANN module ("HLint: ignore Use cs" :: String) #-}

-- do not insert more than 4 elems into a Style set
instance HasEnoughInhabitants (Atom Style) where numOfInhabitants _ = Just 4

instance GenEdit RawContent where
    genEdit d = sizedEdit $ do
            c <- genEdit d
            let doc = rawContentToDoc $ patch c d  -- TUNING: eliminate rawContentToDoc
            e <- genEdit doc
            let doc' = patch e doc
            pure $ c <> eRawContent (e <> makeJoinEdits doc')

--------------------------------------------------------- tests

spec :: Spec
spec = parallel $ do

    runTest $ fastTests @RawContent
    -- TUNING: speed up diff on RawContent to be able to use runTest
    runTest' (RunTestConfig (Just 2) (Just 10)) $ hardTests @RawContent

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` d

    it "RawContent <-> Doc conversion" . property $ \d -> do
      docToRawContent (rawContentToDoc d) `shouldBe` d


    describe "showEditAsRawContent" $ do
      let block0 = BlockIndex 0 $ BlockKey "0"

      describe "added text with custom style 'ADDED'." $ do
        let edit = eRawContent [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit . EditItem 0 . editSecond $ coerce
                        [ InsertItem 10 'a'
                        , InsertItem 11 'n'
                        , InsertItem 12 'd'
                        , InsertItem 13 '/']]]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text and/or other" & blockStyles .~ [(EntityRange 10 4, StyleAdded)]) :| []
            ranges = mconcat $ rangesFromRange True <$> [Range (Position block0 10) (Position block0 10)]
        it "show diff" $ showEditAsRawContent edit rc `shouldBe` rc'
        it "ranges" $ docEditRanges edit rc `shouldBe` ranges

      describe "deleted text with custom style 'DELETED'." $ do
        let -- FIXME: edit rc . mkRawContent $ mkBlock "someer" :| []
            -- this doesn't work now because the cost of deleting chars is more than
            -- the cost of deleting the block and adding a new one
            edit = eRawContent [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit . EditItem 0 . editSecond
                        . coerce $ deleteRange 4 12]]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [(EntityRange 4 12, StyleDeleted)]) :| []
            ranges = mconcat $ rangesFromRange False <$> [Range (Position block0 4) (Position block0 16)]
        it "show diff" $ showEditAsRawContent edit rc `shouldBe` rc'
        it "ranges" $ docEditRanges edit rc `shouldBe` ranges

      describe "deleted block with custom style 'DELETED'." $ do
        let edit = eRawContent $ ENonEmpty <$> deleteRange 0 1
            rc   = mkRawContent $ mkBlock "some text or other" :| [mkBlock "x"]
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [(EntityRange 0 18, StyleDeleted)]) :| [mkBlock "x"]
            ranges = mconcat $ rangesFromRange False <$> [Range (Position block0 0) (Position block0 18)]
        it "show diff" $ showEditAsRawContent edit rc `shouldBe` rc'
        it "ranges" $ docEditRanges edit rc `shouldBe` ranges

      it "hideUnchangedParts" $ do
        let toRC = docToRawContent . NEL.fromList . map toBlock
            wipeBlockKeys = rawContentBlocks %~ fmap (blockKey .~ BlockKey "0")
            toBlock '.' = DocBlock NormalText (BlockDepth 0) (block0 ^. blockIndexKey) [((Atom Nothing, mempty), "...")]
            toBlock c   = DocBlock NormalText (BlockDepth 0) (block0 ^. blockIndexKey)
                            [((Atom Nothing, Set.fromList [Atom StyleChanged | isUpper c]), fromString [c])]
        wipeBlockKeys (hideUnchangedParts (toRC "aBcdefGH") 0 0) `shouldBe` wipeBlockKeys (toRC ".B.GH")
        wipeBlockKeys (hideUnchangedParts (toRC "aBcdefGH") 1 1) `shouldBe` wipeBlockKeys (toRC "aBc.fGH")
        wipeBlockKeys (hideUnchangedParts (toRC "aBcdefGH") 2 1) `shouldBe` wipeBlockKeys (toRC "aBc.efGH")
        wipeBlockKeys (hideUnchangedParts (toRC "aBcdefGH") 1 2) `shouldBe` wipeBlockKeys (toRC "aBcd.fGH")
        wipeBlockKeys (hideUnchangedParts (toRC "aBcdefGH") 2 2) `shouldBe` wipeBlockKeys (toRC "aBcdefGH")

    describe "transformRange" $ do
      let mkRC = NEL.fromList . zipWith mkBl [0..] . lines
          mkBl i = DocBlock NormalText (BlockDepth 0) (bKey i) . mkLE
          mkLE
            = map (fst . head &&& NonEmptyST . (cs :: String -> ST) . map snd)
            . groupBy ((==) `on` fst)
            . map mkChar
          mkChar = \case
            c | isLower c -> ((Atom Nothing, Set.fromList []), c)
              | isUpper c -> ((Atom Nothing, Set.fromList [Atom Bold]), c)
            _ -> error "mkChar"

          bKey = BlockKey . cs . show
          pos r = pos' r r
          pos' r k = Position . BlockIndex r $ bKey k

      it "empty edit" $ do
        transformRangeOTDoc [] (mkRC "aaa") (Range (pos 0 0) (pos 0 0)) `shouldBe` Range (pos 0 0) (pos 0 0)
      it "insert row" $ do
        let edit = [ENonEmpty . InsertItem 1 $ mkBl 10 "xyz"]
        transformRangeOTDoc edit (mkRC "a\nb") (Range (pos 0 0) (pos 0 0)) `shouldBe` Range (pos 0 0) (pos 0 0)
        transformRangeOTDoc edit (mkRC "a\nb") (Range (pos 0 0) (pos 1 0)) `shouldBe` Range (pos 0 0) (pos' 2 1 0)

      it "delete rows" $ do
        let edit = [ENonEmpty $ DeleteRange 1 2]
        transformRangeOTDoc edit (mkRC "aa\nbb\ncc\ndd") (Range (pos 0 1) (pos 3 1)) `shouldBe` Range (pos 0 1) (pos' 1 3 1)
        transformRangeOTDoc edit (mkRC "aa\nbb\ncc\ndd") (Range (pos 1 1) (pos 3 1)) `shouldBe` Range (pos' 1 3 0) (pos' 1 3 1)
        transformRangeOTDoc edit (mkRC "aa\nbb\ncc\ndd") (Range (pos 0 1) (pos 2 1)) `shouldBe` Range (pos 0 1) (pos 0 2)

      it "delete rows (2)" $ do
        let edit = [ENonEmpty $ DeleteRange 0 2]
        transformRangeOTDoc edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 3 1)) `shouldBe` Range (pos' 0 2 0) (pos' 1 3 1)
        transformRangeOTDoc edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 2 1)) `shouldBe` Range (pos' 0 2 0) (pos' 0 2 1)
        transformRangeOTDoc edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 1 1)) `shouldBe` Range (pos' 0 2 0) (pos' 0 2 0)

      describe "insert line elem" $ do
        let edit = [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit . InsertItem 1 . head $ mkLE "bcd"]]
        it "1" $ transformRangeOTDoc edit (mkRC "AAaa\na") (Range (pos 0 1) (pos 0 3)) `shouldBe` Range (pos 0 1) (pos 0 6)
        it "2" $ transformRangeOTDoc edit (mkRC "AAaa\na") (Range (pos 0 2) (pos 0 3)) `shouldBe` Range (pos 0 2{-FIXME: 5-}) (pos 0 6)
        it "3" $ transformRangeOTDoc edit (mkRC "AAaa\na") (Range (pos 0 3) (pos 0 3)) `shouldBe` Range (pos 0 6) (pos 0 6)
        it "4" $ transformRangeOTDoc edit (mkRC "AAaa\na") (Range (pos 0 1) (pos 0 2)) `shouldBe` Range (pos 0 1) (pos 0 2)
        it "5" $ transformRangeOTDoc edit (mkRC "AAaa\na") (Range (pos 0 1) (pos 0 1)) `shouldBe` Range (pos 0 1) (pos 0 1)

      let mkTests as = sequence_ . zipWith3 (\i a -> it (show i) . shouldBe a) [1::Int ..] as

      describe "delete line elems" $ do
        let edit = [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit $ DeleteRange 1 1]]
            doc = mkRC "AabB"
        mkTests (map (transformRangeOTDoc edit doc) [Range (pos 0 x) (pos 0 y) | x <- [0..4], y <- [x..4]])
          [ Range (pos 0 0) (pos 0 0)
          , Range (pos 0 0) (pos 0 1)
          , Range (pos 0 0) (pos 0 1)
          , Range (pos 0 0) (pos 0 1)
          , Range (pos 0 0) (pos 0 2)

          , Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 2)

          , Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 2)

          , Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 2)

          , Range (pos 0 2) (pos 0 2)
          ]

      describe "insert characters" $ do
        let edit = [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit . EditItem 1 $ editSecond
                        [ NEText . EText $ InsertItem 1 'c'
                        , NEText . EText $ InsertItem 2 'd']]]
            doc = mkRC "AabB"
        mkTests (map (transformRangeOTDoc edit doc) [Range (pos 0 x) (pos 0 y) | x <- [1..3], y <- [x..3]])
          [ Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 4{-FIXME: 2-})
          , Range (pos 0 1) (pos 0 5)

          , Range (pos 0 4) (pos 0 4) -- alternative solution: Range (pos 0 2) (pos 0 2)
          , Range (pos 0 4) (pos 0 5)

          , Range (pos 0 5) (pos 0 5)
          ]

      describe "delete characters" $ do
        let edit = [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit . EditItem 1 $ editSecond
                        [ NEText . EText $ DeleteRange 1 1 ]]]
            doc = mkRC "AabcB"
        mkTests (map (transformRangeOTDoc edit doc) [Range (pos 0 x) (pos 0 y) | x <- [1..4], y <- [x..4]])
          [ Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 2)
          , Range (pos 0 1) (pos 0 2)
          , Range (pos 0 1) (pos 0 3)

          , Range (pos 0 2) (pos 0 2)
          , Range (pos 0 2) (pos 0 2)
          , Range (pos 0 2) (pos 0 3)

          , Range (pos 0 2) (pos 0 2)
          , Range (pos 0 2) (pos 0 3)

          , Range (pos 0 3) (pos 0 3)
          ]

      describe "edit characters" $ do
        let edit = [ENonEmpty . EditItem 0 $ editSecond [SegmentListEdit . EditItem 1 $ editSecond
                        [ NEText . EText $ EditItem 1 [EChar $ EAtom 'x'] ]]]
            doc = mkRC "AabcA"
        mkTests (map (transformRangeOTDoc edit doc) [Range (pos 0 x) (pos 0 y) | x <- [1..4], y <- [x..4]])
          [ Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 2)
          , Range (pos 0 1) (pos 0 3)
          , Range (pos 0 1) (pos 0 4)

          , Range (pos 0 2) (pos 0 2)
          , Range (pos 0 2) (pos 0 3)
          , Range (pos 0 2) (pos 0 4)

          , Range (pos 0 3) (pos 0 3)
          , Range (pos 0 3) (pos 0 4)

          , Range (pos 0 4) (pos 0 4)
          ]
