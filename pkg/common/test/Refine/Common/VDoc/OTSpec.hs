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
import           Test.QuickCheck
import           Test.Hspec

import Refine.Common.OTSpec hiding (spec)
import Refine.Common.Test.Arbitrary
import Refine.Common.OT
import Refine.Common.VDoc.OT
import Refine.Common.Types.Core
import Refine.Common.VDoc.Draft


-- | Block canonicalization: remove empty line elems; merge neighboring line elems with same attr set.
simplifyDoc :: OTDoc -> OTDoc
simplifyDoc blocks = simplifyBlock <$> blocks
  where
    simplifyBlock (DocBlock a d b) = DocBlock a d (map joinElems $ groupBy ((==) `on` attrs) b)

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) . mconcat $ map txt xs

-- do not insert more than 4 elems into a Style set
instance HasEnoughInhabitants (Atom Style) where numOfInhabitants _ = Just 4

instance GenEdit RawContent where
    genEdit d = (map ERawContent <$> genEdit (rawContentToDoc d)) `suchThat` \edit -> isValidRawContent (patch edit d)
      where
        isValidRawContent rc = sanitizeRawContent rc == rc

--------------------------------------------------------- tests

spec :: Spec
spec = parallel $ do

    -- pendingWith "#310"
    -- runTest $ allTests @RawContent

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` simplifyDoc d

    it "RawContent <-> Doc conversion" . property $ \d -> do
      let clear = sanitizeRawContent . resetBlockKeys
      (clear . docToRawContent . rawContentToDoc) d `shouldBe` clear d


    describe "showEditAsRawContent" $ do
      it "shows added text with custom style 'ADDED'." $ do
        let edit = [ERawContent . ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
                        [ NEText (InsertItem 10 'a')
                        , NEText (InsertItem 11 'n')
                        , NEText (InsertItem 12 'd')
                        , NEText (InsertItem 13 '/')]]]]]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text and/or other" & blockStyles .~ [((10, 4), StyleAdded)]) :| []
        showEditAsRawContent edit rc `shouldBe` rc'

      it "shows deleted text with custom style 'DELETED'." $ do
        let -- FIXME: edit rc . mkRawContent $ mkBlock "someer" :| []
            -- this doesn't work now because the cost of deleting chars is more than
            -- the cost of deleting the block and adding a new one
            edit = [ERawContent . ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
                        [ NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)
                        , NEText (DeleteItem 4)]]]]]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [((4, 12), StyleDeleted)]) :| []
        showEditAsRawContent edit rc `shouldBe` rc'

      it "shows deleted block with custom style 'DELETED'." $ do
        let edit = [ERawContent . ENonEmpty $ DeleteItem 0]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [((0, 18), StyleDeleted)]) :| []
        showEditAsRawContent edit rc `shouldBe` rc'
