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
import           Test.QuickCheck
import           Test.Hspec

import Refine.Common.OTSpec hiding (spec)
import Refine.Common.Test.Arbitrary
import Refine.Common.OT
import Refine.Common.VDoc.OT
import Refine.Common.Types.Core hiding (Edit)
import Refine.Common.VDoc.Draft


-- | Block canonicalization: merge neighboring line elems with same attr set.
makeJoinEdits :: OTDoc -> Edit OTDoc
makeJoinEdits blocks = concat . zipWith simplifyBlock [0..] $ NEL.toList blocks
  where
    simplifyBlock :: Int -> DocBlock -> Edit OTDoc
    simplifyBlock i (DocBlock _ _ _ ls) = map ENonEmpty . editItem i . editSecond $ case ls of
        []  -> []
        [_] -> []
        (es, _): xs -> go 0 es xs

    go _ _ [] = []
    go i p ((es, _): ls)
        | p == es = JoinItems i: go i es ls
        | otherwise = go (i+1) es ls

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

    -- TODO: review
    it "RawContent <-> Doc conversion" . property $ \d -> do
      let clear = sanitizeRawContent . resetBlockKeys
      (clear . docToRawContent . rawContentToDoc) d `shouldBe` clear d


    describe "showEditAsRawContent" $ do
      it "shows added text with custom style 'ADDED'." $ do
        let edit = eRawContent [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
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
            edit = eRawContent [ENonEmpty $ EditItem 0 [EditSecond [SegmentListEdit $ EditItem 0 [EditSecond
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
        let edit = eRawContent [ENonEmpty $ DeleteItem 0]
            rc   = mkRawContent $ mkBlock "some text or other" :| []
            rc'  = mkRawContent $ (mkBlock "some text or other" & blockStyles .~ [((0, 18), StyleDeleted)]) :| []
        showEditAsRawContent edit rc `shouldBe` rc'
