{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}

module Draft
where

import           Data.Foldable (toList)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.String.Conversions
import           GHC.Generics

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

