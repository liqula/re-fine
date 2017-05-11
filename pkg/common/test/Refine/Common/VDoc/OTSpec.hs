{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.VDoc.OTSpec where

import qualified Data.Set as Set
import           Test.QuickCheck
import           Test.Hspec

import Refine.Common.OT
import Refine.Common.OTSpec hiding (spec)
import Refine.Common.Test.Arbitrary
import Refine.Common.VDoc.OT
import qualified Refine.Common.VDoc.Draft as Draft


---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter

instance GenEdit Draft.BlockType where
    genEdit d = map EBlockType <$> genEdit (from d)

----------------------

instance Arbitrary Entity where
    arbitrary = garbitrary

instance HasEnoughInhabitants Entity where hasMoreInhabitantsThan _ _ = True

instance GenEdit Entity where
    genEdit d = map EEntity <$> genEdit (from d)

----------------------

instance Arbitrary LineElem where
    arbitrary = LineElem <$> attrs <*> arbitrary
      where
        attrs = Set.fromList <$> do
            n <- elements [0..10]
            vectorOf n arbitrary

instance GenEdit LineElem where
    genEdit d = map ELineElem <$> genEdit (from d)

----------------------

instance Arbitrary Block where
    arbitrary = garbitrary

instance GenEdit Block where
    genEdit d = map EBlock <$> genEdit (from d)

----------------------

instance Arbitrary Doc where
    arbitrary = Doc <$> ((:) <$> arbitrary <*> arbitrary)  -- RawContent block list must not be empty!

instance GenEdit Doc where
    genEdit d = map EDoc <$> genEdit (from d)

----------------------

instance GenEdit Draft.RawContent where
    genEdit d = map ERawContent <$> genEdit (from d)

--------------------------------------------------------- tests

spec :: Spec
spec = parallel $ do
    runTest $ allTests @Draft.BlockType
    runTest $ allTests @LineElem

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` simplifyDoc d

    it "RawContent <-> Doc conversion" . property $ \d -> do
      let clear = sanitizeRawContent . Draft.resetBlockKeys
      (clear . docToRawContent . rawContentToDoc) d `shouldBe` clear d

    it "RawContent <-> Doc conversion" $ do
      let d = Draft.samples !! 0 in docToRawContent (rawContentToDoc d) `shouldBe` Draft.resetBlockKeys d

    it "RawContent <-> Doc conversion" $ do
      let d = Draft.samples !! 1 in docToRawContent (rawContentToDoc d) `shouldBe` Draft.resetBlockKeys d
