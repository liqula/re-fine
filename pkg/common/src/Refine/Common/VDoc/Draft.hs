{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}

module Refine.Common.VDoc.Draft
where

import           Control.Exception (assert)
import           Data.Aeson
import           Data.Foldable (toList)
import           Data.Functor.Infix ((<$$>))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.String.Conversions


-- * data types

-- | Haskell representation of the javascript @RawDraftContentState@.
-- https://draftjs.org/docs/api-reference-data-conversion.html#content
data RawContent = RawContent
  { _rawContentBlocks    :: [Block EntityKey]
  , _rawContentEntityMap :: IntMap Entity  -- ^ for performance, do not use @Map EntityKey Entity@ here.
  }
  deriving (Eq, Show)

-- | typical rangekey values are 'Int' and 'Entity'
data Block rangeKey = Block
  { _blockText         :: ST
  , _blockEntityRanges :: [(rangeKey, EntityRange)]
  , _blockStyles       :: [(EntityRange, Style)]
  , _blockType         :: BlockType
  , _blockKey          :: Maybe BlockKey
  }
  deriving (Eq, Show, Functor, Foldable)

-- | `key` attribute of the 'Block'.  'SelectionState' uses this to refer to blocks.  If in doubt
-- leave it 'Nothing'.
newtype BlockKey = BlockKey ST
  deriving (Eq, Show, ToJSON, FromJSON)

-- | key into 'rawContentEntityMap'.
newtype EntityKey = EntityKey { _unEntityKey :: Int }
  deriving (Eq, Show, ToJSON, FromJSON)

type EntityRange = (Int, Int)

-- | an entity's range may span across multiple blocks
data Entity =
    EntityLink ST  -- ^ url
--  | ...
  deriving (Show, Eq, Ord)

-- | a style's range should fit into a single block
data Style =
    Bold
  | Italic
  deriving (Show, Eq)

-- | each block has a unique blocktype
data BlockType =
    NormalText  -- FUTUREWORK: add depth
  | Header1
  | Header2
  | Header3
  | BulletPoint Int -- ^ depth
  | EnumPoint   Int -- ^ depth
  deriving (Show, Eq)


-- * instances

instance ToJSON RawContent where
  toJSON (RawContent blocks entitymap) = object
    [ "blocks"    .= blocks
    , "entityMap" .= object [ cs (show a) .= b | (a, b) <- IntMap.toList entitymap ]
    ]

instance FromJSON RawContent where
  parseJSON = assert False undefined

instance ToJSON (Block EntityKey) where
  toJSON (Block content ranges styles ty key) = object $
    [ "text"              .= content
    , "entityRanges"      .= (renderRanges <$> ranges)
    , "inlineStyleRanges" .= (renderStyles <$> styles)
    , "depth"             .= blockTypeDepth ty
    , "type"              .= ty
    ] <>
    [ "key" .= k | k <- maybeToList key ]
    where
      renderRanges (k, (l, o))    = object ["key"   .= k, "length" .= l, "offset" .= o]
      renderStyles ((l, o), s)    = object ["style" .= s, "length" .= l, "offset" .= o]

instance ToJSON BlockType where
  toJSON NormalText      = "unstyled"
  toJSON Header1         = "header-one"
  toJSON Header2         = "header-two"
  toJSON Header3         = "header-three"
  toJSON (BulletPoint _) = "unordered-list-item"
  toJSON (EnumPoint _)   = "ordered-list-item"

instance ToJSON Entity where
  toJSON (EntityLink url) = object
    [ "type"            .= ("LINK" :: ST)
    , "mutability"      .= ("MUTABLE" :: ST)
    , "data"            .= object ["url" .= url]
    ]

instance ToJSON Style where
  toJSON Bold   = "BOLD"
  toJSON Italic = "ITALIC"


-- * functions

mkRawContent :: [Block Entity] -> RawContent
mkRawContent bs = RawContent (index <$$> bs) (IntMap.fromList entities)
  where
    -- FUTUREWORK: it is possible to do just one traversal to collect and index entities
    -- https://www.reddit.com/r/haskell/comments/610sa1/applicative_sorting/
    entities = zip [0..] . nub $ concatMap toList bs

    index :: Entity -> EntityKey
    index e = EntityKey . fromMaybe (error "mkRawContent: impossible") $ Map.lookup e em

    em = Map.fromList $ (\(a, b) -> (b, a)) <$> entities

blockTypeDepth :: BlockType -> Int
blockTypeDepth (BulletPoint d) = d
blockTypeDepth (EnumPoint d)   = d
blockTypeDepth _               = 0
