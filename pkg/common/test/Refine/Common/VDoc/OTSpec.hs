{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.VDoc.OTSpec where

import Test.QuickCheck
import Test.Hspec
import Data.List

import Refine.Common.OT
import Refine.Common.OTSpec hiding (spec)
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

instance HasEnoughElems Entity where hasMoreElemsThan _ _ = True

instance GenEdit Entity where
    genEdit d = map EEntity <$> genEdit (from d)

----------------------

instance Arbitrary LineElem where
    arbitrary = LineElem <$> attrs <*> arbitrary
      where
        attrs = Set . nub . sort <$> do
            n <- elements [0,1,2,3]
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
    arbitrary = to <$> arbitrary

instance GenEdit Doc where
    genEdit d = map EDoc <$> genEdit (from d)

----------------------

instance Arbitrary RawContent where
    arbitrary = to <$> arbitrary

instance GenEdit RawContent where
    genEdit d = map ERawContent <$> genEdit (from d)

--------------------------------------------------------- tests

test_transform :: Doc -> Bool
test_transform d = rawContentToDoc (docToRawContent d) == simplifyDoc d

spec :: Spec
spec = parallel $ do
    runTest $ allTests @(Atom HeaderLevel)
    runTest $ allTests @(Atom ItemType)
    runTest $ allTests @BlockType
    runTest $ allTests @LineElem
    --FIXME: to slow -- it "Doc <-> RawContent conversion" $ property test_transform
