{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}   -- FIXME: elim this
module Doc where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.IntMap as IntMap
import           Test.QuickCheck

import OT hiding (runTests)
import Draft (RawContent)
import qualified Draft as Draft

---------------------------------------- auxiliary document data type

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

---------------------------------------- conversion functions
{- segments
  ------            bold
     --------       italic
xxxxxxxxxxxxxxxxxxxxxx converted to line elements:
  ---               bold
     ---            bold + italic
        -----       italic

 xxx [xxXXXx](www.1) xxxxx
 xxx [xx](www.1)[XXX](www.1)[x](www.1) xxxxx
-}

rawContentToDoc :: Draft.RawContent -> Doc
rawContentToDoc (Draft.RawContent blocks entities) = Doc $ mkBlock <$> blocks
  where
    fromEntity :: Draft.Entity -> Entity
    fromEntity (Draft.EntityLink s) = EntityLink s

    fromStyle :: Draft.Style -> Entity
    fromStyle = \case
        Draft.Bold   -> EntityBold
        Draft.Italic -> EntityItalic

    mkBlockType :: Int -> Draft.BlockType -> BlockType
    mkBlockType d = \case
        Draft.Header1     -> Header HL1
        Draft.Header2     -> Header HL2
        Draft.Header3     -> Header HL3
        Draft.NormalText  -> Item NormalText  d
        Draft.BulletPoint -> Item BulletPoint d
        Draft.EnumPoint   -> Item EnumPoint   d

    mkBlock :: Draft.Block Draft.EntityKey -> Block
    mkBlock (Draft.Block txt eranges styles ty depth _key) = Block (mkBlockType depth ty) (segment segments txt)
      where
        segment [] "" = []
        segment [] text = [LineElem mempty text]
        segment ((len, s): ss) text = LineElem (Set $ sort s) (take len text): segment ss (drop len text)

        segments =
              mkSegments 0 []
            . map (\((beg, end), s) -> (beg, (end, s)))
            . sortBy (compare `on` fst)
            $ [(r, fromEntity $ entities IntMap.! k) | (Draft.EntityKey k, r) <- eranges] ++ [(r, fromStyle s) | (r, s) <- styles]

        mkSegments _ [] [] = []
        mkSegments n stack ((n', z): ss) | n' == n = mkSegments n (insertBy (compare `on` fst) z stack) ss
        mkSegments n ((n', _): stack) ss | n' == n = mkSegments n stack ss
        mkSegments n stack ss
            | n' > n = (n' - n, snd <$> stack): mkSegments n' stack ss
            | otherwise = error "impossible"
          where
            n' = case (fst <$> stack, fst <$> ss) of
                (a: _, b: _) -> min a b
                (a: _, _)    -> a
                (_, b: _)    -> b
                _            -> error "impossible"

docToRawContent :: Doc -> Draft.RawContent
docToRawContent (Doc blocks) = Draft.mkRawContent $ mkBlock <$> blocks
  where
    toEntity :: Entity -> Maybe Draft.Entity
    toEntity = \case
        EntityLink s -> Just (Draft.EntityLink s)
        _ -> Nothing

    toStyle :: Entity -> Maybe Draft.Style
    toStyle = \case
        EntityBold   -> Just Draft.Bold
        EntityItalic -> Just Draft.Italic
        _ -> Nothing

    mkType = \case
        Header HL1         -> (Draft.Header1, 0)
        Header HL2         -> (Draft.Header2, 0)
        Header HL3         -> (Draft.Header3, 0)
        Item NormalText  d -> (Draft.NormalText,  d)
        Item BulletPoint d -> (Draft.BulletPoint, d)
        Item EnumPoint   d -> (Draft.EnumPoint,   d)

    getText (LineElem _ txt) = txt

    mkBlock :: Block -> Draft.Block Draft.Entity
    mkBlock (Block ty es) = Draft.Block
        (concatMap getText es)
        [(e, r) | (r, toEntity -> Just e) <- ranges]
        [(r, s) | (r, toStyle  -> Just s) <- ranges]
        (fst $ mkType ty)
        (snd $ mkType ty)
        Nothing
      where
        ranges = mkRanges 0 mempty $ [(length txt, unSet s) | LineElem s txt <- es] ++ [(0, mempty)]

        mkRanges _ [] [] = []
        mkRanges n acc ((len, s): ss)
            = [((beg, n), sty) | (beg, sty) <- acc, sty `notElem` s, beg /= n]
            ++ mkRanges (n + len)
                        (foldr (insertBy (compare `on` fst))
                               [(beg, sty) | (beg, sty) <- acc, sty `elem` s]
                               [(n, sty) | sty <- s, sty `notElem` (map snd acc)])
                        ss
        mkRanges _ _ _ = error "impossible"

test_transform :: Doc -> Bool
test_transform d = rawContentToDoc (docToRawContent d) == simplifyDoc d

simplifyDoc :: Doc -> Doc
simplifyDoc (Doc blocks) = Doc $ simplifyBlock <$> blocks
  where
    simplifyBlock (Block a b) = Block a $ filter notNull $ map joinElems $ groupBy ((==) `on` attrs) b

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) $ concatMap txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter

instance Arbitrary HeaderLevel where
    arbitrary = elements [minBound..]

instance Arbitrary ItemType where
    arbitrary = elements [minBound..]


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

----------------------

instance Representable RawContent where
    type Rep RawContent = Doc
    to = docToRawContent
    from = rawContentToDoc

instance Arbitrary RawContent where
    arbitrary = to <$> arbitrary

instance Editable RawContent where

    newtype EEdit RawContent
        = ERawContent {unERawContent :: EEdit (Rep RawContent)}
      deriving (Show)

    docCost = docCost . from
    eCost = eCost . unERawContent
    diff a b = map ERawContent $ diff (from a) (from b)
    ePatch e = to . ePatch (unERawContent e) . from
    eMerge d a b = map ERawContent *** map ERawContent $ eMerge (from d) (unERawContent a) (unERawContent b)
    eInverse d = map ERawContent . eInverse (from d) . unERawContent

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

---------------------- data type used for testing

runTests :: IO ()
runTests = do
    runTest 1000 $ allTests @(Atom HeaderLevel)
    runTest 1000 $ allTests @(Atom ItemType)
    runTest 1000 $ allTests @BlockType
    runTest 1000 $ allTests @LineElem
