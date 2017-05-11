{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}   -- FIXME: elim this
module Refine.Common.VDoc.OT where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.String.Conversions
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)
import           Data.Aeson

import Refine.Common.OT
import Refine.Common.VDoc.Draft (RawContent)
import qualified Refine.Common.VDoc.Draft as Draft
import Refine.Prelude.TH (makeJSON)

---------------------------------------- auxiliary document data type

newtype Doc = Doc [Block]
   deriving (Show, Eq, Generic)

-- | (third constructor arg is depth)
data Block = Block Draft.BlockType [LineElem] Int
   deriving (Show, Eq, Generic)

-- | A segment of an inline style, consisting of 'EntityRange' and 'Style'.
--
-- FIXME: (Set Entity) should be (Maybe String, Bool, Bool), it is not allowed to have two links on
-- the same character.
data LineElem = LineElem (Set.Set Entity) ST
   deriving (Show, Eq, Generic)

newtype Entity = Entity { unEntity :: Either Draft.Entity Draft.Style }
   deriving (Show, Eq, Ord, Generic)

---------------------------------------- 

data EditSource a =
    InitialEdit
  | EditOfEdit (Edit RawContent) a
  | MergeOfEdits a a
  deriving (Show, Functor)

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
    -- see also: 'Refine.Common.VDoc.Draft.addMarksToBlock'.
    mkBlock :: Draft.Block Draft.EntityKey -> Block
    mkBlock (Draft.Block txt eranges styles ty depth _key) = Block ty (segment segments txt) depth
      where
        segment [] "" = []
        segment [] text = [LineElem mempty text]
        segment ((len, s): ss) text = LineElem s (Text.take len text): segment ss (Text.drop len text)

        segments =
              mkSegments 0 []
            . map (\((offset, len), s) -> (offset, (offset + len, s)))
            . sortBy (compare `on` fst)
            $ [(r, Entity . Left $ entities IntMap.! k) | (Draft.EntityKey k, r) <- eranges] <> [(r, Entity $ Right s) | (r, s) <- styles]

        -- TODO: stack (2nd arg) should be @IntMap (Set style)@ (keyed by @offset@).
        mkSegments _ [] [] = []  -- (stack will be emptied in the third case.)
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
    getText (LineElem _ txt) = txt

    mkBlock :: Block -> Draft.Block Draft.Entity
    mkBlock (Block ty es d) = Draft.Block
        (mconcat $ map getText es)
        [(e, r) | (r, unEntity -> Left e) <- ranges]
        [(r, s) | (r, unEntity -> Right s) <- ranges]
        ty
        d
        Nothing
      where
        ranges = mkRanges 0 mempty
            $ [(len, s) | LineElem s txt <- es, let len = Text.length txt, len > 0]
            <> [(0, mempty)]  -- this is to avoid one more case in mkRanges below when we're done.

        mkRanges _ _ [] = []
        mkRanges n acc ((len, s): ss)
            = -- construct all non-empty ranges that are closed in s
              [((offset, l), sty) | (offset, sty) <- acc, sty `Set.notMember` s, let l = n - offset, l > 0]
           <> -- jump to the next segment (aka line element)
              mkRanges (n + len)
                        (  [(offset, sty) | (offset, sty) <- acc, sty `Set.member` s]
                        <> [(n, sty) | sty <- Set.elems s, sty `notElem` map snd acc])
                        ss

-- | Block canonicalization: remove empty line elems; merge neighboring line elems with same attr set.
simplifyDoc :: Doc -> Doc
simplifyDoc (Doc blocks) = Doc $ simplifyBlock <$> blocks
  where
    simplifyBlock (Block a b d) = Block a (map joinElems . groupBy ((==) `on` attrs) $ filter notNull b) d

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) . mconcat $ map txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

------------------------------------------------------- auxiliary definitions

class Representable a where
  type Rep a
  to   :: Rep a -> a
  from :: a -> Rep a

---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter


instance Representable Draft.BlockType where
    type Rep Draft.BlockType = Atom Draft.BlockType
    to = unAtom
    from = Atom

instance Editable Draft.BlockType where
    newtype EEdit Draft.BlockType
        = EBlockType {unEBlockType :: EEdit (Rep Draft.BlockType)}
      deriving (Generic, Show)

    docCost = docCost . from
    eCost = eCost . unEBlockType
    diff a b = map EBlockType $ diff (from a) (from b)
    ePatch e = to . ePatch (unEBlockType e) . from
    eMerge d a b = map EBlockType *** map EBlockType $ eMerge (from d) (unEBlockType a) (unEBlockType b)
    eInverse d = map EBlockType . eInverse (from d) . unEBlockType

instance ToJSON (EEdit Draft.BlockType)
instance FromJSON (EEdit Draft.BlockType)

----------------------

instance Representable Entity where
    type Rep Entity = Atom Entity
    to = unAtom
    from = Atom

instance Editable Entity where
    newtype EEdit Entity
        = EEntity {unEEntity :: EEdit (Rep Entity)}
      deriving (Generic, Show)

    docCost = docCost . from
    eCost = eCost . unEEntity
    diff a b = map EEntity $ diff (from a) (from b)
    ePatch e = to . ePatch (unEEntity e) . from
    eMerge d a b = map EEntity *** map EEntity $ eMerge (from d) (unEEntity a) (unEEntity b)
    eInverse d = map EEntity . eInverse (from d) . unEEntity

instance ToJSON (EEdit Entity)
instance FromJSON (EEdit Entity)

----------------------

instance Representable LineElem where
    type Rep LineElem = (Set.Set Entity, ST)
    to (a, b) = LineElem a b
    from (LineElem a b) = (a, b)

instance Editable LineElem where
    newtype EEdit LineElem
        = ELineElem {unELineElem :: EEdit (Rep LineElem)}
        -- FUTUREWORK: detect and be able to merge joining and splitting of 'LineElem's
      deriving (Generic, Show)

    docCost = docCost . from
    eCost = eCost . unELineElem
    diff a b = map ELineElem $ diff (from a) (from b)
    ePatch e = to . ePatch (unELineElem e) . from
    eMerge d a b = map ELineElem *** map ELineElem $ eMerge (from d) (unELineElem a) (unELineElem b)
    eInverse d = map ELineElem . eInverse (from d) . unELineElem

instance ToJSON (EEdit LineElem)
instance FromJSON (EEdit LineElem)

----------------------

instance Representable Block where
    type Rep Block = ((Draft.BlockType, [LineElem]), Atom Int)
    to ((a, b), Atom d) = Block a b d
    from (Block a b d) = ((a, b), Atom d)

instance Editable Block where
    newtype EEdit Block
        = EBlock {unEBlock :: EEdit (Rep Block)}
        -- FUTUREWORK: detect and be able to merge joining and splitting of 'Block's
      deriving (Generic, Show)

    docCost = docCost . from
    eCost = eCost . unEBlock
    diff a b = map EBlock $ diff (from a) (from b)
    ePatch e = to . ePatch (unEBlock e) . from
    eMerge d a b = map EBlock *** map EBlock $ eMerge (from d) (unEBlock a) (unEBlock b)
    eInverse d = map EBlock . eInverse (from d) . unEBlock

instance ToJSON (EEdit Block)
instance FromJSON (EEdit Block)

----------------------

instance Representable Doc where
    type Rep Doc = [Block]
    to = Doc
    from (Doc a) = a

instance Editable Doc where
    newtype EEdit Doc
        = EDoc {unEDoc :: EEdit (Rep Doc)}
      deriving (Generic, Show)

    docCost = docCost . from
    eCost = eCost . unEDoc
    diff a b = map EDoc $ diff (from a) (from b)
    ePatch e = to . ePatch (unEDoc e) . from
    eMerge d a b = map EDoc *** map EDoc $ eMerge (from d) (unEDoc a) (unEDoc b)
    eInverse d = map EDoc . eInverse (from d) . unEDoc

instance ToJSON (EEdit Doc)
instance FromJSON (EEdit Doc)

----------------------

instance Representable RawContent where
    type Rep RawContent = Doc
    to = docToRawContent
    from = rawContentToDoc

instance Editable RawContent where
    newtype EEdit RawContent
        = ERawContent {unERawContent :: EEdit (Rep RawContent)}
      deriving (Generic, Show)

    docCost = docCost . from
    eCost = eCost . unERawContent
    diff a b = map ERawContent $ diff (from a) (from b)
    ePatch e = to . ePatch (unERawContent e) . from
    eMerge d a b = map ERawContent *** map ERawContent $ eMerge (from d) (unERawContent a) (unERawContent b)
    eInverse d = map ERawContent . eInverse (from d) . unERawContent

instance ToJSON (EEdit RawContent)
instance FromJSON (EEdit RawContent)

instance SOP.Generic Doc
instance SOP.Generic Block
instance SOP.Generic LineElem
instance SOP.Generic Entity

instance SOP.HasDatatypeInfo Doc
instance SOP.HasDatatypeInfo Block
instance SOP.HasDatatypeInfo LineElem
instance SOP.HasDatatypeInfo Entity

makeJSON ''Doc
makeJSON ''Block
makeJSON ''LineElem
makeJSON ''Entity
