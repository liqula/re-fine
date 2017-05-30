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
simplifyDoc [] = [DocBlock NormalText 0 []]
simplifyDoc blocks = simplifyBlock <$> blocks
  where
    simplifyBlock (DocBlock a d b) = DocBlock a d (map joinElems . groupBy ((==) `on` attrs) $ filter notNull b)

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) . mconcat $ map txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

-- do not insert more than 5 elems into an EntityStyle set
instance HasEnoughInhabitants (Atom EntityStyle) where numOfInhabitants _ = Just 5

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
        let edit = [ERawContent $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
                        [ EText (InsertItem 10 'a')
                        , EText (InsertItem 11 'n')
                        , EText (InsertItem 12 'd')
                        , EText (InsertItem 13 '/')]]]]]
            rc   = mkRawContent [mkBlock "some text or other"]
            rc'  = mkRawContent [mkBlock "some text and/or other" & blockStyles .~ [((10, 4), StyleAdded)]]
        showEditAsRawContent edit rc `shouldBe` rc'

      it "shows deleted text with custom style 'DELETED'." $ do
        let -- FIXME: edit rc $ mkRawContent [mkBlock "someer"]
            -- this doesn't work now because the cost of deleting chars is more than
            -- the cost of deleting the block and adding a new one
            edit = [ERawContent $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
                        [ EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)
                        , EText (DeleteItem 4)]]]]]
            rc   = mkRawContent [mkBlock "some text or other"]
            rc'  = mkRawContent [mkBlock "some text or other" & blockStyles .~ [((4, 12), StyleDeleted)]]
        showEditAsRawContent edit rc `shouldBe` rc'

      it "shows deleted block with custom style 'DELETED'." $ do
        let edit = [ERawContent $ DeleteItem 0]
            rc   = mkRawContent [mkBlock "some text or other"]
            rc'  = mkRawContent [mkBlock "some text or other" & blockStyles .~ [((0, 18), StyleDeleted)]]
        showEditAsRawContent edit rc `shouldBe` rc'
