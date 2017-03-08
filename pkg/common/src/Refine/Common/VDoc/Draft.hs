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

module Refine.Common.VDoc.Draft
where

import           Data.Aeson
import           Data.String.Conversions
import           Data.List ((\\))
import qualified Data.Text as ST
import           Data.Tree
import           Text.HTML.Parser
import           Text.HTML.Tree

import Refine.Common.Types


-- * target data types

data RawContent = RawContent { _rawContentBlocks :: [Block Int] }

mkRawContent :: [Block ()] -> RawContent
mkRawContent bs = RawContent $ go 0 bs
  where
    go _ [] = []
    go n (Block text ranges styles depth : blocks) = block' : blocks'
      where
        blocks' = go n' blocks
        n'      = n + length ranges
        block'  = Block text ranges' styles depth
        ranges' = zipWith (\i (_, r, e) -> (i, r, e)) [n..] ranges

-- | ('rangeKey' must be 'Int' if you want to construct 'RawContent', but it can be '()' if you want
-- to just float around 'Blocks' without knowing about the other blocks and the order yet.)
data Block rangeKey = Block
  { _blockText         :: ST
  , _blockEntityRanges :: [(rangeKey, EntityRange, Entity)]
  , _blockStyles       :: [(EntityRange, Style)]
  , _blockDepth        :: Int
  }

-- | (a bit like 'ChunkRange', but also different.)
type EntityRange = (Int, Int)

data Entity  -- (we don't need this yet.)

data Style =
    Header1
  | Header2
  | Header3
  | Bold
  | Italic
  | BulletPoint
  | EnumPoint
  deriving (Eq)


instance ToJSON RawContent where
  toJSON (RawContent blocks) = object
    [ "blocks" .= (toJSON <$> blocks)
    , "entityMap" .= mkEntityMap blocks
    ]

instance ToJSON (Block Int) where
  toJSON (Block content ranges styles depth) = object
    [ "text"              .= content
    , "entityRanges"      .= (renderRanges <$> ranges)
    , "inlineStyleRanges" .= (renderStyles <$> styles)
    , "depth"             .= depth
    ]
    where
      renderRanges (k, (l, o), _) = object ["key"   .= k, "length" .= l, "offset" .= o]
      renderStyles ((l, o), s)    = object ["style" .= s, "length" .= l, "offset" .= o]

data EntityMap = EntityMap

mkEntityMap :: [Block Int] -> EntityMap
mkEntityMap _ = EntityMap

instance ToJSON EntityMap where
  toJSON _ = object []

instance ToJSON Style where
  toJSON Header1 = "HEADER1"
  toJSON Header2 = "HEADER2"
  toJSON Header3 = "HEADER3"
  toJSON Bold = "BOLD"
  toJSON Italic = "ITALIC"
  toJSON BulletPoint = "BULLETPOINT"
  toJSON EnumPoint = "ENUMPOINT"


-- * conversion

vdocVersionToRawContent :: VDocVersion 'HTMLCanonical -> RawContent
vdocVersionToRawContent (VDocVersion forest) = mkRawContent . fmap cons . merge . decons [] . tokensFromForest $ forest
  where
    -- TODO: decons is very lenient re. unexpected input, and does not generate all the styles yet.
    -- TODO: allow for styles on parts of blocks (currently every style change opens a new block).
    decons :: [Style] -> [Token] -> [([Style], ST)]
    decons styles (TagOpen n as   : tokens) = decons (styleOn n as styles) tokens
    decons styles (TagClose n     : tokens) = decons (styleOff n styles) tokens
    decons styles (ContentText st : tokens) = (styles, st) : decons styles tokens
    decons styles _                         = []

    styleOn :: TagName -> [Attr] -> [Style] -> [Style]
    styleOn "h1"     _ = (Header1:)
    styleOn "h2"     _ = (Header2:)
    styleOn "h3"     _ = (Header3:)
    styleOn "strong" _ = (Bold:)
    styleOn _        _ = id

    styleOff :: TagName -> [Style] -> [Style]
    styleOff "h1"     = (\\ [Header1])
    styleOff "h2"     = (\\ [Header2])
    styleOff "h3"     = (\\ [Header3])
    styleOff "strong" = (\\ [Bold])
    styleOff _        = id

    merge :: [([Style], ST)] -> [([Style], ST)]
    merge = id

    cons :: ([Style], ST) -> Block ()
    cons (styles, st) = Block st [] ((range,) <$> styles) 0
      where
        range = (0, ST.length st - 1)


-- TODO: i am wondering at this point if we really want to have the html rep.  what about
-- always using draft, even for the view mode?
