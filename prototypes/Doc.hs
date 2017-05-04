{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeApplications           #-}
module Doc where

import Control.Arrow
import Test.QuickCheck

import OT hiding (runTests)

---------------------------------------- document data type

newtype Doc = Doc [Block]
   deriving (Show, Eq)

data Block = Block BlockType [LineElem]
   deriving (Show, Eq)

data BlockType =
     Header HeaderLevel
   | Item ItemType Int -- depth
   deriving (Show, Eq)

data HeaderLevel
    = HL1 | HL2 | HL3
   deriving (Show, Eq, Bounded, Enum)

data ItemType = NormalText | BulletPoint | EnumPoint
   deriving (Show, Eq, Bounded, Enum)

-- | This is something which is described with an EntityRange
data LineElem = LineElem (Set Entity{-should be Map, it is not allowed to have two links on the same character-}) String
   deriving (Show, Eq)

-- | This is both Entity and Style in Draft
data Entity
    = EntityLink String
    | EntityBold
    | EntityItalic
   deriving (Show, Eq, Ord)

---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter


instance Representable BlockType where
    type Rep BlockType = Either (Atom HeaderLevel) (Atom ItemType, Atom Int)
    to = either (Header . unAtom) (uncurry Item . (unAtom *** unAtom))
    from = \case
        Header s -> Left $ Atom s
        Item a b -> Right (Atom a, Atom b)

instance Arbitrary BlockType where
    arbitrary = to <$> arbitrary

instance Editable BlockType where

    newtype EEdit BlockType
        = EBlockType {unEBlockType :: EEdit (Rep BlockType)}
      deriving (Show)

    docCost = docCost . from
    eCost = eCost . unEBlockType
    diff a b = map EBlockType $ diff (from a) (from b)
    ePatch e = to . ePatch (unEBlockType e) . from
    eMerge d a b = map EBlockType *** map EBlockType $ eMerge (from d) (unEBlockType a) (unEBlockType b)
    eInverse d = map EBlockType . eInverse (from d) . unEBlockType

--instance HasEnoughElems BlockType where hasMoreElemsThan _ _ = True

instance GenEdit BlockType where
    genEdit d = map EBlockType <$> genEdit (from d)

----------------------

instance Representable Entity where
    type Rep Entity = Either [Atom Char] (Either () ())
    to = either (EntityLink . map unAtom) (either (const EntityBold) (const EntityItalic))
    from = \case
        EntityLink s -> Left $ Atom <$> s
        EntityBold   -> Right (Left ())
        EntityItalic -> Right (Right ())

instance Arbitrary Entity where
    arbitrary = to <$> arbitrary

instance Editable Entity where

    newtype EEdit Entity
        = EEntity {unEEntity :: EEdit (Rep Entity)}
      deriving (Show)

    docCost = docCost . from
    eCost = eCost . unEEntity
    diff a b = map EEntity $ diff (from a) (from b)
    ePatch e = to . ePatch (unEEntity e) . from
    eMerge d a b = map EEntity *** map EEntity $ eMerge (from d) (unEEntity a) (unEEntity b)
    eInverse d = map EEntity . eInverse (from d) . unEEntity

instance HasEnoughElems Entity where hasMoreElemsThan _ _ = True

instance GenEdit Entity where
    genEdit d = map EEntity <$> genEdit (from d)

----------------------

instance Representable LineElem where
    type Rep LineElem = (Set Entity, String)
    to (a, b) = LineElem a b
    from (LineElem a b) = (a, b)

instance Arbitrary LineElem where
    arbitrary = to <$> arbitrary

instance Editable LineElem where

    newtype EEdit LineElem
        = ELineElem {unELineElem :: EEdit (Rep LineElem)}
        -- FUTUREWORK: detect and be able to merge joining and splitting of lineelems
      deriving (Show)

    docCost = docCost . from
    eCost = eCost . unELineElem
    diff a b = map ELineElem $ diff (from a) (from b)
    ePatch e = to . ePatch (unELineElem e) . from
    eMerge d a b = map ELineElem *** map ELineElem $ eMerge (from d) (unELineElem a) (unELineElem b)
    eInverse d = map ELineElem . eInverse (from d) . unELineElem

instance GenEdit LineElem where
    genEdit d = map ELineElem <$> genEdit (from d)

----------------------

instance Representable Block where
    type Rep Block = (BlockType, [LineElem])
    to (a, b) = Block a b
    from (Block a b) = (a, b)

instance Arbitrary Block where
    arbitrary = to <$> arbitrary

instance Editable Block where

    newtype EEdit Block
        = EBlock {unEBlock :: EEdit (Rep Block)}
        -- FUTUREWORK: detect and be able to merge joining and splitting of blocks
      deriving (Show)

    docCost = docCost . from
    eCost = eCost . unEBlock
    diff a b = map EBlock $ diff (from a) (from b)
    ePatch e = to . ePatch (unEBlock e) . from
    eMerge d a b = map EBlock *** map EBlock $ eMerge (from d) (unEBlock a) (unEBlock b)
    eInverse d = map EBlock . eInverse (from d) . unEBlock

instance GenEdit Block where
    genEdit d = map EBlock <$> genEdit (from d)

----------------------

instance Representable Doc where
    type Rep Doc = [Block]
    to = Doc
    from (Doc a) = a

instance Arbitrary Doc where
    arbitrary = to <$> arbitrary

instance Editable Doc where

    newtype EEdit Doc
        = EDoc {unEDoc :: EEdit (Rep Doc)}
      deriving (Show)

    docCost = docCost . from
    eCost = eCost . unEDoc
    diff a b = map EDoc $ diff (from a) (from b)
    ePatch e = to . ePatch (unEDoc e) . from
    eMerge d a b = map EDoc *** map EDoc $ eMerge (from d) (unEDoc a) (unEDoc b)
    eInverse d = map EDoc . eInverse (from d) . unEDoc

instance GenEdit Doc where
    genEdit d = map EDoc <$> genEdit (from d)

------------------

doc1, doc2 :: Doc
doc1 = Doc
    [ Block (Header HL1) [LineElem mempty "Intro"]
    , Block (Item NormalText 0) [LineElem mempty "This is"]
    ]

doc2 = Doc
    [ Block (Header HL1) [LineElem mempty "Intro"]
    , Block (Item NormalText 0) [LineElem (Set [EntityBold]) "This", LineElem mempty " is"]
    ]

---------------------- data type used for testing

runTests :: IO ()
runTests = do
    runTest 1000 $ allTests @(Atom HeaderLevel)
    runTest 1000 $ allTests @(Atom ItemType)
    runTest 1000 $ allTests @BlockType
    runTest 1000 $ allTests @LineElem
