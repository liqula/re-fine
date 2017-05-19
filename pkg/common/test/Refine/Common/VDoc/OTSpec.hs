{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
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
simplifyDoc [] = [DocBlock NormalText [] 0]
simplifyDoc blocks = simplifyBlock <$> blocks
  where
    simplifyBlock (DocBlock a b d) = DocBlock a (map joinElems . groupBy ((==) `on` attrs) $ filter notNull b) d

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) . mconcat $ map txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

-- do not insert more than 5 elems into an EntityStyle set
instance HasEnoughInhabitants (Atom EntityStyle) where numOfInhabitants _ = Just 5

instance GenEdit RawContent where
    genEdit d = map ERawContent <$> genEdit (rawContentToDoc d)

--------------------------------------------------------- tests

spec :: Spec
spec = parallel $ do

    runTest $ allTests @RawContent

    -- if this take too long to run on a regular basis, just activate for debugging or deep-tests:
    -- runTest $ allTests @RawContent

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` simplifyDoc d

    it "RawContent <-> Doc conversion" . property $ \d -> do
      let clear = sanitizeRawContent . resetBlockKeys
      (clear . docToRawContent . rawContentToDoc) d `shouldBe` clear d


    describe "showEditAsRawContent" $ do
      let styleAdded = StyleAdded
          styleDeleted = StyleDeleted
          --styleChanged = StyleChanged

      it "shows added text with custom style 'ADDED'." $ do
        let edit = [ERawContent $ EditItem 0 [EditFirst [EditSecond [EditItem 0 [EditSecond
                        [ EText (InsertItem 10 'a')
                        , EText (InsertItem 11 'n')
                        , EText (InsertItem 12 'd')
                        , EText (InsertItem 13 '/')]]]]]]
            rc   = mkRawContent [mkBlock "some text or other"]
            rc'  = mkRawContent [mkBlock "some text and/or other" & blockStyles .~ [((10, 4), styleAdded)]]
        showEditAsRawContent edit rc `shouldBe` rc'

      it "shows deleted text with custom style 'DELETED'." $ do
        let -- FIXME: edit rc $ mkRawContent [mkBlock "someer"]
            -- this doesn't work now because the cost of deleting chars is more than
            -- the cost of deleting the block and adding a new one
            edit = [ERawContent $ EditItem 0 [EditFirst [EditSecond [EditItem 0 [EditSecond
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
                        , EText (DeleteItem 4)]]]]]]
            rc   = mkRawContent [mkBlock "some text or other"]
            rc'  = mkRawContent [mkBlock "some text or other" & blockStyles .~ [((4, 12), styleDeleted)]]
        showEditAsRawContent edit rc `shouldBe` rc'

      it "shows deleted block with custom style 'DELETED'." $ do
        let edit = [ERawContent $ DeleteItem 0]
            rc   = mkRawContent [mkBlock "some text or other"]
            rc'  = mkRawContent [mkBlock "some text or other" & blockStyles .~ [((0, 18), styleDeleted)]]
        showEditAsRawContent edit rc `shouldBe` rc'
