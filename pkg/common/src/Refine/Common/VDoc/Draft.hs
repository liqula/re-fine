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

import           Control.Lens (Lens', (^.), (.~), (&))
import           Control.Monad (foldM)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Foldable (toList)
import           Data.Functor.Infix ((<$$>))
import qualified Data.HashMap.Lazy as HashMap
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.String.Conversions
import           GHC.Generics

import Refine.Prelude.TH


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
  , _blockKey          :: Maybe BlockKey
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

-- | `key` attribute of the 'Block'.  'SelectionState' uses this to refer to blocks.  If in doubt
-- leave it 'Nothing'.
newtype BlockKey = BlockKey ST
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

-- | key into 'rawContentEntityMap'.
newtype EntityKey = EntityKey { _unEntityKey :: Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

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
    NormalText  -- FUTUREWORK: add depth
  | Header1
  | Header2
  | Header3
  | BulletPoint Int -- ^ depth
  | EnumPoint   Int -- ^ depth
  deriving (Show, Eq, Generic)


-- | https://draftjs.org/docs/api-reference-selection-state.html
data SelectionState
  = Selection
      { _selectionStart :: SelectionPoint
      , _selectionEnd   :: SelectionPoint
      }
  deriving (Eq, Ord, Generic)

data SelectionPoint
  = SelectionPoint
      { _selectionBlock  :: BlockKey
      , _selectionOffset :: Int
      }
  deriving (Eq, Ord, Generic)


-- * instances

makeSOPGeneric ''RawContent
makeSOPGeneric ''Block
makeSOPGeneric ''BlockKey
makeSOPGeneric ''EntityKey
makeSOPGeneric ''Entity
makeSOPGeneric ''Style
makeSOPGeneric ''BlockType
makeSOPGeneric ''SelectionState
makeSOPGeneric ''SelectionPoint

makeNFData ''RawContent
makeNFData ''Block
makeNFData ''BlockKey
makeNFData ''EntityKey
makeNFData ''Entity
makeNFData ''Style
makeNFData ''BlockType
makeNFData ''SelectionState
makeNFData ''SelectionPoint

instance ToJSON RawContent where
  toJSON (RawContent blocks entitymap) = object
    [ "blocks"    .= blocks
    , "entityMap" .= renderEntityMap entitymap
    ]
    where
      renderEntityMap m = object [ cs (show a) .= b | (a, b) <- IntMap.toList m ]

instance FromJSON RawContent where
  parseJSON = withObject "RawContent" $ \obj -> RawContent
    <$> obj .: "blocks"
    <*> (parseEntityMap =<< obj .: "entityMap")
    where
      parseEntityMap = withObject "parseEntityMap" $ foldM f mempty . HashMap.toList
        where
          f :: IntMap Entity -> (ST, Value) -> Parser (IntMap Entity)
          f m (read . cs -> k, v) = (\e -> IntMap.insert k e m) <$> parseJSON v

instance ToJSON (Block EntityKey) where
  toJSON (Block content ranges styles ty key) = object $
    [ "text"              .= content
    , "entityRanges"      .= (renderRange <$> ranges)
    , "inlineStyleRanges" .= (renderStyle <$> styles)
    , "depth"             .= (ty ^. blockTypeDepth)
    , "type"              .= ty
    ] <>
    [ "key" .= k | k <- maybeToList key ]
    where
      renderRange (k, (l, o)) = object ["key"   .= k, "length" .= l, "offset" .= o]
      renderStyle ((l, o), s) = object ["style" .= s, "length" .= l, "offset" .= o]

instance FromJSON (Block EntityKey) where
  parseJSON = withObject "Block EntityKey" $ \obj -> Block
    <$> obj .: "text"
    <*> (mapM parseRange =<< (obj .: "entityRanges"))
    <*> (mapM parseStyle =<< (obj .: "inlineStyleRanges"))
    <*> getType obj
    <*> obj .:? "key"
    where
      parseRange = withObject "Block EntityKey: entityRanges" $ \obj -> do
        k <- obj .: "key"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure (k, (l, o))
      parseStyle = withObject "Block EntityKey: inlineStyleRanges" $ \obj -> do
        s <- obj .: "style"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure ((l, o), s)

      getType obj = do
        d <- obj .: "depth"
        t <- obj .: "type"
        pure (t & blockTypeDepth .~ d)

instance ToJSON BlockType where
  toJSON NormalText      = "unstyled"
  toJSON Header1         = "header-one"
  toJSON Header2         = "header-two"
  toJSON Header3         = "header-three"
  toJSON (BulletPoint _) = "unordered-list-item"
  toJSON (EnumPoint _)   = "ordered-list-item"

instance FromJSON BlockType where
  parseJSON (String "unstyled")            = pure NormalText
  parseJSON (String "header-one")          = pure Header1
  parseJSON (String "header-two")          = pure Header2
  parseJSON (String "header-three")        = pure Header3
  parseJSON (String "unordered-list-item") = pure $ BulletPoint 0
  parseJSON (String "ordered-list-item")   = pure $ EnumPoint 0
  parseJSON bad = fail $ "BlockType: no parse for " <> show bad

instance ToJSON Entity where
  toJSON (EntityLink url) = object
    [ "type"            .= ("LINK" :: ST)
    , "mutability"      .= ("MUTABLE" :: ST)
    , "data"            .= object ["url" .= url]
    ]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \obj -> do
    ty :: ST <- obj .: "type"
    case ty of
      "LINK" -> let parseData = withObject "LINK data" (.: "url")
                in EntityLink <$> (parseData =<< obj .: "data")
      bad -> fail $ "Entity: no parse for " <> show bad

instance ToJSON Style where
  toJSON Bold   = "BOLD"
  toJSON Italic = "ITALIC"

instance FromJSON Style where
  parseJSON (String "BOLD")   = pure Bold
  parseJSON (String "ITALIC") = pure Italic
  parseJSON bad = fail $ "Style: no parse for " <> show bad


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

-- | TODO: this is a Prism, not a Lens.  that's why the test case fails.
blockTypeDepth :: Lens' BlockType Int
blockTypeDepth focus = \case
  NormalText    -> const NormalText <$> focus 0
  Header1       -> const Header1    <$> focus 0
  Header2       -> const Header2    <$> focus 0
  Header3       -> const Header3    <$> focus 0
  BulletPoint d -> BulletPoint      <$> focus d
  EnumPoint d   -> EnumPoint        <$> focus d
