{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Refine.Backend.Database.Types where

import           GHC.Generics (Generic)

import Refine.Common.Types.Chunk (ChunkPoint)
import Refine.Prelude.TH (makeRefineType)


data DBChunkRange = DBChunkRange
  { _dbChunkRangeBegin :: Maybe ChunkPoint
  , _dbChunkRangeEnd   :: Maybe ChunkPoint
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeRefineType ''DBChunkRange
