{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}

module Draft
where

import           Data.Function
import           Data.Foldable (toList)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics

import qualified Doc
import qualified OT as Doc

type ST = String

-- * data types

-- | Haskell representation of the javascript @RawDraftContentState@.
-- https://draftjs.org/docs/api-reference-data-conversion.html#content
data RawContent = RawContent
  { _rawContentBlocks    :: [Block EntityKey]
  , _rawContentEntityMap :: IntMap Entity  -- ^ for performance, do not use @Map EntityKey Entity@ here.
  }
  deriving (Eq, Show, Generic)

-- | typical rangekey values are 'Int' and 'Entity'
data Block rangeKey = Block
  { _blockText         :: ST
  , _blockEntityRanges :: [(rangeKey, EntityRange)]
  , _blockStyles       :: [(EntityRange, Style)]
  , _blockType         :: BlockType
  , _blockDepth        :: Int
  , _blockKey          :: Maybe BlockKey
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

-- | `key` attribute of the 'Block'.  'SelectionState' uses this to refer to blocks.  If in doubt
-- leave it 'Nothing'.
newtype BlockKey = BlockKey ST
  deriving (Eq, Ord, Show)

-- | key into 'rawContentEntityMap'.
newtype EntityKey = EntityKey { _unEntityKey :: Int }
  deriving (Eq, Ord, Show)

type EntityRange = (Int, Int)

-- | an entity's range may span across multiple blocks
newtype Entity =
    EntityLink ST  -- ^ url
--  | ...
  deriving (Show, Eq, Ord, Generic)

-- | a style's range should fit into a single block
data Style =
    Bold
  | Italic
  deriving (Show, Eq, Generic)

-- | each block has a unique blocktype
data BlockType =
    NormalText
  | Header1
  | Header2
  | Header3
  | BulletPoint
  | EnumPoint
  deriving (Show, Eq, Generic)


mkRawContent :: [Block Entity] -> RawContent
mkRawContent bs = RawContent (fmap index <$> bs) (IntMap.fromList entities)
  where
    -- FUTUREWORK: it is possible to do just one traversal to collect and index entities
    -- https://www.reddit.com/r/haskell/comments/610sa1/applicative_sorting/
    entities = zip [0..] . nub $ concatMap toList bs

    index :: Entity -> EntityKey
    index e = EntityKey . fromMaybe (error "mkRawContent: impossible") $ Map.lookup e em

    em = Map.fromList $ (\(a, b) -> (b, a)) <$> entities

----------------------------------------------
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


rawContentToDoc :: RawContent -> Doc.Doc
rawContentToDoc (RawContent blocks entities) = Doc.Doc $ mkBlock <$> blocks
  where
    fromEntity :: Entity -> Doc.Entity
    fromEntity (EntityLink s) = Doc.EntityLink s

    fromStyle :: Style -> Doc.Entity
    fromStyle = \case
        Bold   -> Doc.EntityBold
        Italic -> Doc.EntityItalic

    mkBlockType :: Int -> BlockType -> Doc.BlockType
    mkBlockType d = \case
        Header1     -> Doc.Header Doc.HL1
        Header2     -> Doc.Header Doc.HL2
        Header3     -> Doc.Header Doc.HL3
        NormalText  -> Doc.Item Doc.NormalText  d
        BulletPoint -> Doc.Item Doc.BulletPoint d
        EnumPoint   -> Doc.Item Doc.EnumPoint   d

    mkBlock :: Block EntityKey -> Doc.Block
    mkBlock (Block txt eranges styles ty depth _key) = Doc.Block (mkBlockType depth ty) (segment segments txt)
      where
        segment [] "" = []
        segment [] text = [Doc.LineElem mempty text]
        segment ((len, s): ss) text = Doc.LineElem (Doc.Set $ sort s) (take len text): segment ss (drop len text)

        segments =
              mkSegments 0 []
            . sortBy (compare `on` fst)
            . map (\((beg, end), s) -> (beg, (end, s)))
            $ [(r, fromEntity $ entities IntMap.! k) | (EntityKey k, r) <- eranges] ++ [(r, fromStyle s) | (r, s) <- styles]

        mkSegments n stack ((n', z): ss) | n' == n = mkSegments n (insertBy (compare `on` fst) z stack) ss
        mkSegments n ((n', _): stack) ss | n' == n = mkSegments n stack ss
        mkSegments _ [] [] = []
        mkSegments n stack ss = (n' - n, snd <$> stack): mkSegments n' stack ss
          where
            n' = case (fst <$> stack, fst <$> ss) of
                (a: _, b: _) -> min a b
                (a: _, _)    -> a
                (_, b: _)    -> b
                _            -> error "impossible"

docToRawContent :: Doc.Doc -> RawContent
docToRawContent (Doc.Doc blocks) = mkRawContent $ mkBlock <$> blocks
  where
    toEntity :: Doc.Entity -> Maybe Entity
    toEntity = \case
        Doc.EntityLink s -> Just (EntityLink s)
        _ -> Nothing

    toStyle :: Doc.Entity -> Maybe Style
    toStyle = \case
        Doc.EntityBold   -> Just Bold
        Doc.EntityItalic -> Just Italic
        _ -> Nothing

    mkType = \case
        Doc.Header Doc.HL1         -> (Header1, 0)
        Doc.Header Doc.HL2         -> (Header2, 0)
        Doc.Header Doc.HL3         -> (Header3, 0)
        Doc.Item Doc.NormalText  d -> (NormalText,  d)
        Doc.Item Doc.BulletPoint d -> (BulletPoint, d)
        Doc.Item Doc.EnumPoint   d -> (EnumPoint,   d)

    getText (Doc.LineElem _ txt) = txt

    mkBlock :: Doc.Block -> Block Entity
    mkBlock (Doc.Block ty es) = Block
        (concatMap getText es)
        [(e, r) | (r, toEntity -> Just e) <- ranges]
        [(r, s) | (r, toStyle  -> Just s) <- ranges]
        (fst $ mkType ty)
        (snd $ mkType ty)
        Nothing
      where
        ranges =
            mkRanges 0 mempty $ [(length txt, Doc.unSet s) | Doc.LineElem s txt <- es] ++ [(0, mempty)]

        mkRanges _ [] [] = []
        mkRanges n acc ((len, s): ss)
            = [((beg, n), sty) | (beg, sty) <- acc, sty `notElem` s]
            ++ mkRanges (n + len) [(beg, sty) | (beg, sty) <- acc, sty `elem` s] ss
        mkRanges _ _ _ = error "impossible"
