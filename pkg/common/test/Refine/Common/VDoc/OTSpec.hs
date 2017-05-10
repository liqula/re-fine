{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.VDoc.OTSpec where

import qualified Data.Set as Set
import           Test.QuickCheck
import           Test.Hspec

import Refine.Common.OT
import Refine.Common.OTSpec hiding (spec)
import Refine.Common.Test.Arbitrary ()
import Refine.Common.VDoc.OT
import Refine.Common.VDoc.Draft (RawContent)


---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter

instance Arbitrary HeaderLevel where
    arbitrary = elements [minBound..]

instance Arbitrary ItemType where
    arbitrary = elements [minBound..]

instance Arbitrary BlockType where
    arbitrary = to <$> arbitrary

instance GenEdit BlockType where
    genEdit d = map EBlockType <$> genEdit (from d)

----------------------

instance Arbitrary Entity where
    arbitrary = to <$> arbitrary

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
    arbitrary = to <$> arbitrary

instance GenEdit Block where
    genEdit d = map EBlock <$> genEdit (from d)

----------------------

instance Arbitrary Doc where
    arbitrary = to <$> ((:) <$> arbitrary <*> arbitrary)  -- RawContent block list must not be empty!

instance GenEdit Doc where
    genEdit d = map EDoc <$> genEdit (from d)

----------------------

instance GenEdit RawContent where
    genEdit d = map ERawContent <$> genEdit (from d)

--------------------------------------------------------- tests

spec :: Spec
spec = parallel $ do
    runTest $ allTests @(Atom HeaderLevel)
    runTest $ allTests @(Atom ItemType)
    runTest $ allTests @BlockType
    runTest $ allTests @LineElem

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` simplifyDoc d

    it "RawContent <-> Doc conversion" . property $ \d ->
      docToRawContent (rawContentToDoc d) `shouldBe` d
