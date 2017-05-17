{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}   -- FIXME: elim this
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- pattern completeness checker has problems with pattern synonyms
module Refine.Common.VDoc.OT where

import Refine.Common.Prelude

import qualified Data.IntMap as IntMap
import           Data.List
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import           Control.DeepSeq

import           Refine.Common.OT
import           Refine.Common.VDoc.Draft (RawContent)
import qualified Refine.Common.VDoc.Draft as Draft

---------------------------------------- auxiliary document data type

type Doc = [Block]

-- | (third constructor arg is depth)
type Block = ((Atom Draft.BlockType, [LineElem]), Atom Int)

pattern Block :: Draft.BlockType -> [LineElem] -> Int -> Block
pattern Block a b c = ((Atom a, b), Atom c)

-- | A segment of an inline style, consisting of 'EntityRange' and 'Style'.
--
-- FIXME: (Set Entity) should be (Maybe String, Bool, Bool), it is not allowed to have two links on
-- the same character.
type LineElem = (Set (Atom Entity), ST)

-- | TUNING: (Set.mapMonotonic unAtom) and (Set.mapMonotonic Atom) should be (coerce)
--   but GHC can't see that (Ord (Atom a)) is the same as (Ord a)
pattern LineElem :: Set Entity -> ST -> LineElem
pattern LineElem a b <- (Set.mapMonotonic unAtom -> a, b)
  where LineElem a b = (Set.mapMonotonic Atom a, b)


type Entity = Either Draft.Entity Draft.Style

----------------------------------------

data EditSource a =
    InitialEdit
  | EditOfEdit (Edit RawContent) a
  | MergeOfEdits a a
  deriving (Show, Functor)

---------------------------------------- conversion functions

rawContentToDoc :: Draft.RawContent -> Doc
rawContentToDoc (Draft.RawContent blocks entities) = mkBlock <$> blocks
  where
    mkBlock :: Draft.Block Draft.EntityKey -> Block
    mkBlock (Draft.Block txt eranges styles ty depth _key) = Block ty (segment segments txt) depth
      where
        segment [] "" = []
        segment [] text = [LineElem mempty text]
        segment ((len, s): ss) text = LineElem s (ST.take len text): segment ss (ST.drop len text)

        segments :: [(Int, Set Entity)]
        segments = Draft.mkSomeSegments fst snd
                 $ (second Right <$> styles)
                <> ((\(e, r) -> (r, Left (entities IntMap.! coerce e))) <$> eranges)

docToRawContent :: Doc -> Draft.RawContent
docToRawContent blocks = Draft.mkRawContent $ mkBlock <$> blocks
  where
    getText (LineElem _ txt) = txt

    mkBlock :: Block -> Draft.Block Draft.Entity
    mkBlock (Block ty es d) = Draft.Block
        (mconcat $ map getText es)
        [(e, r) | (r, Left e) <- ranges]
        [(r, s) | (r, Right s) <- ranges]
        ty
        d
        Nothing
      where
        ranges = mkRanges 0 mempty
            $ [(len, s) | LineElem s txt <- es, let len = ST.length txt, len > 0]
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
simplifyDoc [] = [Block Draft.NormalText [] 0]
simplifyDoc blocks = simplifyBlock <$> blocks
  where
    simplifyBlock (Block a b d) = Block a (map joinElems . groupBy ((==) `on` attrs) $ filter notNull b) d

    attrs (LineElem x _) = x
    txt   (LineElem _ x) = x

    joinElems xs = LineElem (attrs $ head xs) . mconcat $ map txt xs

    notNull (LineElem _ "") = False
    notNull _ = True

---------------------------------------- Editable instances
-- FUTUREWORK: make these instances smarter

instance Editable RawContent where
    newtype EEdit RawContent
        = ERawContent {unERawContent :: EEdit Doc}
      deriving (Generic, Show, Eq, NFData)

    docCost = docCost . rawContentToDoc
    eCost = eCost . unERawContent

    diff a b = coerce $ diff (rawContentToDoc a) (rawContentToDoc b)
    ePatch e = docToRawContent . ePatch (coerce e) . rawContentToDoc
    patch e = docToRawContent . patch (coerce e) . rawContentToDoc
    eMerge d a b = coerce $ eMerge (rawContentToDoc d) (coerce a) (coerce b)
    merge d a b = coerce $ merge (rawContentToDoc d) (coerce a) (coerce b)
    eInverse d = coerce . eInverse (rawContentToDoc d) . coerce
    inverse d = coerce . inverse (rawContentToDoc d) . coerce

instance ToJSON (EEdit RawContent)
instance FromJSON (EEdit RawContent)
instance SOP.Generic (EEdit RawContent)
instance SOP.HasDatatypeInfo (EEdit RawContent)

