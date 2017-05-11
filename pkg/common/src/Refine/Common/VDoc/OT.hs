{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}   -- FIXME: elim this
module Refine.Common.VDoc.OT where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import           Data.String.Conversions
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)

import Refine.Common.OT
import Refine.Common.VDoc.Draft (RawContent)
import qualified Refine.Common.VDoc.Draft as Draft

---------------------------------------- auxiliary document data type

newtype Doc = Doc [Block]
   deriving (Show, Eq, Generic)

data Block = Block BlockType [LineElem]
   deriving (Show, Eq, Generic)

data BlockType =
     Header HeaderLevel Int -- depth (even though it may not make sense here, the js types still support it).
   | Item ItemType Int -- depth
   deriving (Show, Eq, Generic)

data HeaderLevel
    = HL1 | HL2 | HL3
   deriving (Show, Eq, Bounded, Enum, Generic)

data ItemType = NormalText | BulletPoint | EnumPoint
   deriving (Show, Eq, Bounded, Enum, Generic)

-- | A segment of an inline style, consisting of 'EntityRange' and 'Style'.
--
-- FIXME: (Set Entity) should be (Maybe String, Bool, Bool), it is not allowed to have two links on
-- the same character.
data LineElem = LineElem (Set.Set Entity) String
   deriving (Show, Eq, Generic)

-- | This is both Entity and Style in Draft
data Entity
    = EntityLink String
    | EntityBold
    | EntityItalic
    | EntityUnderline
    | EntityCode
    | EntityRangeComment
    | EntityRangeEdit
   deriving (Show, Eq, Ord, Generic)

---------------------------------------- conversion functions

{- ranges
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
    fromEntity :: Draft.Entity -> Entity  -- FIXME: why don't we use the RawContent type here?
    fromEntity (Draft.EntityLink s) = EntityLink (cs s)

    fromStyle :: Draft.Style -> Entity  -- FIXME: why don't we use the RawContent type here?
    fromStyle = \case
        Draft.Bold         -> EntityBold
        Draft.Italic       -> EntityItalic
        Draft.Underline    -> EntityUnderline
        Draft.Code         -> EntityCode
        Draft.RangeComment -> EntityRangeComment
        Draft.RangeEdit    -> EntityRangeEdit

    mkBlockType :: Int -> Draft.BlockType -> BlockType  -- FIXME: why don't we use the RawContent type here?
    mkBlockType d = \case
        Draft.Header1     -> Header HL1 d
        Draft.Header2     -> Header HL2 d
        Draft.Header3     -> Header HL3 d
        Draft.NormalText  -> Item NormalText  d
        Draft.BulletPoint -> Item BulletPoint d
        Draft.EnumPoint   -> Item EnumPoint   d

    -- see also: 'Refine.Common.VDoc.Draft.addMarksToBlock'.
    mkBlock :: Draft.Block Draft.EntityKey -> Block
    mkBlock (Draft.Block txt eranges styles ty depth _key) = Block (mkBlockType depth ty) (segment segments $ cs txt)
      where
        segment [] "" = []
        segment [] text = [LineElem mempty text]
        segment ((len, s): ss) text = LineElem s (take len text): segment ss (drop len text)

        segments =
              mkSegments 0 []
            . map (\((offset, len), s) -> (offset, (offset + len, s)))
            . sortBy (compare `on` fst)
            $ [(r, fromEntity $ entities IntMap.! k) | (Draft.EntityKey k, r) <- eranges] <> [(r, fromStyle s) | (r, s) <- styles]

        mkSegments _ [] [] = []  -- TODO: why does the stack have to be empty?
        mkSegments n stack ((offset, s): ss) | offset == n = mkSegments n (insertBy (compare `on` fst) s stack) ss
        mkSegments n ((offset, _): stack) ss | offset == n = mkSegments n stack ss
        mkSegments n stack ss                | offset > n  = (offset - n, Set.fromList $ snd <$> stack): mkSegments offset stack ss
          where
            offset = case (fst <$> stack, fst <$> ss) of
                (a: _, b: _) -> min a b
                (a: _, [])   -> a
                ([],   b: _) -> b
                ([],   [])   -> error "impossible"
        mkSegments n stack ss = error $ "impossible: " <> show (n, stack, ss)

docToRawContent :: Doc -> Draft.RawContent
docToRawContent (Doc blocks) = Draft.mkRawContent $ mkBlock <$> blocks
  where
    toEntityStyle :: Entity -> Either Draft.Entity Draft.Style
    toEntityStyle = \case
        EntityLink s        -> Left (Draft.EntityLink $ cs s)
        EntityBold          -> Right Draft.Bold
        EntityItalic        -> Right Draft.Italic
        EntityUnderline     -> Right Draft.Underline
        EntityCode          -> Right Draft.Code
        EntityRangeComment  -> Right Draft.RangeComment
        EntityRangeEdit     -> Right Draft.RangeEdit

    mkType = \case
        Header HL1 d       -> (Draft.Header1, d)
        Header HL2 d       -> (Draft.Header2, d)
        Header HL3 d       -> (Draft.Header3, d)
        Item NormalText  d -> (Draft.NormalText,  d)
        Item BulletPoint d -> (Draft.BulletPoint, d)
        Item EnumPoint   d -> (Draft.EnumPoint,   d)

    getText (LineElem _ txt) = txt

    mkBlock :: Block -> Draft.Block Draft.Entity
    mkBlock (Block ty es) = uncurry
        (Draft.Block
            (cs $ concatMap getText es)
            [(e, r) | (r, toEntityStyle -> Left e) <- ranges]
            [(r, s) | (r, toEntityStyle -> Right s) <- ranges])
        (mkType ty)
        Nothing
      where
        ranges = mkRanges 0 mempty
            $ [(len, s) | LineElem s txt <- es, let len = length txt, len > 0] <> [(0, mempty)]

        mkRanges _ _ [] = []
        mkRanges n acc ((len, s): ss)
            = [((offset, l), sty) | (offset, sty) <- acc, sty `Set.notMember` s, let l = n - offset, l > 0]  -- TODO: why not `member`?
            <> mkRanges (n + len)
                        (  [(offset, sty) | (offset, sty) <- acc, sty `Set.member` s]
                        ++ [(n, sty) | sty <- Set.elems s, sty `notElem` map snd acc])
                        ss

-- | Block canonicalization: remove empty line elems; merge neighboring line elems with same attr set.
simplifyDoc :: Doc -> Doc
simplifyDoc (Doc blocks) = Doc $ simplifyBlock <$> blocks
  where
    simplifyBlock (Block a b) = Block a . map joinElems . groupBy ((==) `on` attrs) $ filter notNull b

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) $ concatMap txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter

instance Representable BlockType where
    type Rep BlockType = Either (Atom HeaderLevel, Atom Int) (Atom ItemType, Atom Int)
    to = either (uncurry Header . (unAtom *** unAtom)) (uncurry Item . (unAtom *** unAtom))
    from = \case
        Header s d -> Left (Atom s, Atom d)
        Item a b   -> Right (Atom a, Atom b)

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
    type Rep Entity = Atom Entity
    to = unAtom
    from = Atom

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
    type Rep LineElem = (Set.Set Entity, String)
    to (a, b) = LineElem a b
    from (LineElem a b) = (a, b)

instance Editable LineElem where
    newtype EEdit LineElem
        = ELineElem {unELineElem :: EEdit (Rep LineElem)}
        -- FUTUREWORK: detect and be able to merge joining and splitting of 'LineElem's
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
        -- FUTUREWORK: detect and be able to merge joining and splitting of 'Block's
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


instance SOP.Generic Doc
instance SOP.Generic Block
instance SOP.Generic BlockType
instance SOP.Generic HeaderLevel
instance SOP.Generic ItemType
instance SOP.Generic LineElem
instance SOP.Generic Entity

instance SOP.HasDatatypeInfo Doc
instance SOP.HasDatatypeInfo Block
instance SOP.HasDatatypeInfo BlockType
instance SOP.HasDatatypeInfo HeaderLevel
instance SOP.HasDatatypeInfo ItemType
instance SOP.HasDatatypeInfo LineElem
instance SOP.HasDatatypeInfo Entity
