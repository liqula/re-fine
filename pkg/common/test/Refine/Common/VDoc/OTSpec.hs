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
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import           Data.Char (isUpper)
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
        let edit = eRawContent [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond $ coerce
                        [ InsertItem 10 'a'
                        , InsertItem 11 'n'
                        , InsertItem 12 'd'
                        , InsertItem 13 '/']]]]]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text and/or other" & blockStyles .~ [(EntityRange 10 4, StyleAdded)]) :| []
            ranges = mconcat $ rangesFromRange True <$> [Range (Position block0 10) (Position block0 10)]
        it "show diff" $ showEditAsRawContent edit rc `shouldBe` rc'
        it "ranges" $ docEditRanges edit rc `shouldBe` ranges

      describe "deleted text with custom style 'DELETED'." $ do
        let -- FIXME: edit rc . mkRawContent $ mkBlock "someer" :| []
            -- this doesn't work now because the cost of deleting chars is more than
            -- the cost of deleting the block and adding a new one
            edit = eRawContent [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
                        . coerce $ deleteRange 4 12]]]]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [(EntityRange 4 12, StyleDeleted)]) :| []
            ranges = mconcat $ rangesFromRange False <$> [Range (Position block0 4) (Position block0 16)]
        it "show diff" $ showEditAsRawContent edit rc `shouldBe` rc'
        it "ranges" $ docEditRanges edit rc `shouldBe` ranges

      describe "deleted block with custom style 'DELETED'." $ do
        let edit = eRawContent $ ENonEmpty <$> deleteRange 0 1
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [(EntityRange 0 18, StyleDeleted)]) :| []
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
            'a' -> ((Atom Nothing, Set.fromList []), 'a')
            'A' -> ((Atom Nothing, Set.fromList [Atom Bold]), 'A')
            _ -> error "mkChar"

          bKey = BlockKey . cs . show
          pos r = pos' r r
          pos' r k = Position . BlockIndex r $ bKey k

      it "empty edit" $ do
        transformRange [] (mkRC "aaa") (Range (pos 0 0) (pos 0 0)) `shouldBe` Range (pos 0 0) (pos 0 0)
      it "insert row" $ do
        let edit = [ENonEmpty . InsertItem 1 $ mkBl 10 "aaa"]
        transformRange edit (mkRC "a\na") (Range (pos 0 0) (pos 0 0)) `shouldBe` Range (pos 0 0) (pos 0 0)
        transformRange edit (mkRC "a\na") (Range (pos 0 0) (pos 1 0)) `shouldBe` Range (pos 0 0) (pos' 2 1 0)

      it "delete rows" $ do
        let edit = [ENonEmpty $ DeleteRange 1 2]
        transformRange edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 3 1)) `shouldBe` Range (pos 0 1) (pos' 1 3 1)
        transformRange edit (mkRC "aa\naa\naa\naa") (Range (pos 1 1) (pos 3 1)) `shouldBe` Range (pos' 1 3 0) (pos' 1 3 1)
        transformRange edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 2 1)) `shouldBe` Range (pos 0 1) (pos 0 2)

      it "delete rows (2)" $ do
        let edit = [ENonEmpty $ DeleteRange 0 2]
        transformRange edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 3 1)) `shouldBe` Range (pos' 0 2 0) (pos' 1 3 1)
        transformRange edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 2 1)) `shouldBe` Range (pos' 0 2 0) (pos' 0 2 1)
        transformRange edit (mkRC "aa\naa\naa\naa") (Range (pos 0 1) (pos 1 1)) `shouldBe` Range (pos' 0 2 0) (pos' 0 2 0)

      it "insert line elem" $ do
        let edit = [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit . InsertItem 1 . head $ mkLE "aaa"]]]
        transformRange edit (mkRC "AAaa\na") (Range (pos 0 1) (pos 0 3)) `shouldBe` Range (pos 0 1) (pos 0 6)
        transformRange edit (mkRC "AAaa\na") (Range (pos 0 2) (pos 0 3)) `shouldBe` Range (pos 0 5) (pos 0 6)
        transformRange edit (mkRC "AAaa\na") (Range (pos 0 3) (pos 0 3)) `shouldBe` Range (pos 0 6) (pos 0 6)
        transformRange edit (mkRC "AAaa\na") (Range (pos 0 1) (pos 0 2)) `shouldBe` Range (pos 0 1) (pos 0 2)
        transformRange edit (mkRC "AAaa\na") (Range (pos 0 1) (pos 0 1)) `shouldBe` Range (pos 0 1) (pos 0 1)

      it "delete line elems" $ do
        let edit = [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ DeleteRange 1 1]]]
            doc = mkRC "AaaA"
        map (transformRange edit doc) [Range (pos 0 x) (pos 0 y) | x <- [0..4], y <- [x..4]] `shouldBe`
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

      it "insert characters" $ do
        let edit = [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 1 [EditSecond
                        [ NEText . EText $ InsertItem 1 'a'
                        , NEText . EText $ InsertItem 2 'a']]]]]
            doc = mkRC "AaaA"
        map (transformRange edit doc) [Range (pos 0 x) (pos 0 y) | x <- [1..3], y <- [x..3]] `shouldBe`
          [ Range (pos 0 1) (pos 0 1)
          , Range (pos 0 1) (pos 0 2)
          , Range (pos 0 1) (pos 0 5)

          , Range (pos 0 2) (pos 0 2)
          , Range (pos 0 4) (pos 0 5)

          , Range (pos 0 5) (pos 0 5)
          ]

      it "delete characters" $ do
        let edit = [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 1 [EditSecond
                        [ NEText . EText $ DeleteRange 1 1 ]]]]]
            doc = mkRC "AaaaA"
        map (transformRange edit doc) [Range (pos 0 x) (pos 0 y) | x <- [1..4], y <- [x..4]] `shouldBe`
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

      it "edit characters" $ do
        let edit = [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 1 [EditSecond
                        [ NEText . EText $ EditItem 1 [EChar $ EAtom 'x'] ]]]]]
            doc = mkRC "AaaaA"
        map (transformRange edit doc) [Range (pos 0 x) (pos 0 y) | x <- [1..4], y <- [x..4]] `shouldBe`
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
