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

import Data.Aeson
import Data.String.Conversions
import Data.Tree
import Text.HTML.Parser
import Text.HTML.Tree

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
    NoStyle
  | Header1
  | Header2
  | Header3
  | Bold
  | Italic
  | BulletPoint
  | EnumPoint


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
  toJSON NoStyle = "NOSTYLE"
  toJSON Header1 = "HEADER1"
  toJSON Header2 = "HEADER2"
  toJSON Header3 = "HEADER3"
  toJSON Bold = "BOLD"
  toJSON Italic = "ITALIC"
  toJSON BulletPoint = "BULLETPOINT"
  toJSON EnumPoint = "ENUMPOINT"


-- * conversion

vdocVersionToRawContent :: VDocVersion 'HTMLCanonical -> RawContent
vdocVersionToRawContent (VDocVersion forest) = mkRawContent $ go forest

go :: Forest Token -> [Block ()]
go [] = []
go _ = []  -- TODO: i am wondering at this point if we really want to have the html rep.  what about
           -- always using draft, even for the view mode?
