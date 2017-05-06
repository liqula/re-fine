{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}   -- FIXME: elim this
module Doc where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.IntMap as IntMap
import           Data.String.Conversions

import OT
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
data LineElem = LineElem (Set Entity) String
    -- FIXME: (Set Entity) should be (Maybe String, Bool, Bool), it is not allowed to have two links on the same character
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
    fromEntity (Draft.EntityLink s) = EntityLink $ cs s

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
    mkBlock (Draft.Block txt eranges styles ty depth _key) = Block (mkBlockType depth ty) (segment segments $ cs txt)
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
        EntityLink s -> Just (Draft.EntityLink $ cs s)
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
        (cs $ concatMap getText es)
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

simplifyDoc :: Doc -> Doc
simplifyDoc (Doc blocks) = Doc $ simplifyBlock <$> blocks
  where
    simplifyBlock (Block a b) = Block a $ map joinElems $ groupBy ((==) `on` attrs) $ filter notNull b

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) $ concatMap txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter

instance Representable BlockType where
    type Rep BlockType = Either (Atom HeaderLevel) (Atom ItemType, Atom Int)
    to = either (Header . unAtom) (uncurry Item . (unAtom *** unAtom))
    from = \case
        Header s -> Left $ Atom s
        Item a b -> Right (Atom a, Atom b)

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

----------------------

instance Representable Entity where
    type Rep Entity = Either [Atom Char] (Either () ())
    to = either (EntityLink . map unAtom) (either (const EntityBold) (const EntityItalic))
    from = \case
        EntityLink s -> Left $ Atom <$> s
        EntityBold   -> Right (Left ())
        EntityItalic -> Right (Right ())

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

----------------------

instance Representable LineElem where
    type Rep LineElem = (Set Entity, String)
    to (a, b) = LineElem a b
    from (LineElem a b) = (a, b)

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

----------------------

instance Representable Block where
    type Rep Block = (BlockType, [LineElem])
    to (a, b) = Block a b
    from (Block a b) = (a, b)

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

----------------------

instance Representable Doc where
    type Rep Doc = [Block]
    to = Doc
    from (Doc a) = a

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

----------------------

instance Representable RawContent where
    type Rep RawContent = Doc
    to = docToRawContent
    from = rawContentToDoc

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

