{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module DocTest where

import           Test.QuickCheck

import OT
import OTTest hiding (runTests)
import Doc
import Draft (RawContent)


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
    arbitrary = to <$> arbitrary

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

doc1, doc2 :: Doc
doc1 = Doc
    [ Block (Header HL1) [LineElem mempty "Intro"]
    , Block (Item NormalText 0) [LineElem mempty "This is"]
    ]

doc2 = Doc
    [ Block (Header HL1) [LineElem mempty "Intro"]
    , Block (Item NormalText 0) [LineElem (Set [EntityBold]) "This", LineElem mempty " is"]
    ]

----------------------

test_transform :: Doc -> Bool
test_transform d = rawContentToDoc (docToRawContent d) == simplifyDoc d

runTests :: IO ()
runTests = do
    runTest 1000 $ allTests @(Atom HeaderLevel)
    runTest 1000 $ allTests @(Atom ItemType)
    runTest 1000 $ allTests @BlockType
    runTest 1000 $ allTests @LineElem
    quickCheck test_transform
