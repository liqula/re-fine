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
