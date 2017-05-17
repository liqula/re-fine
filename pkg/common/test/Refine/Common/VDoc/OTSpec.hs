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

    -- if this take too long to run on a regular basis, just activate for debugging or deep-tests:
    -- runTest $ allTests @RawContent

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` simplifyDoc d

    it "RawContent <-> Doc conversion" . property $ \d -> do
      let clear = sanitizeRawContent . resetBlockKeys
      (clear . docToRawContent . rawContentToDoc) d `shouldBe` clear d
