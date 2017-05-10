{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Refine.Common.Types.Chunk where

import           Data.Functor.Infix ((<$$>))
import           Control.DeepSeq
import           Control.Lens (_1, (%~))
import           Data.Aeson
import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP
import           GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude.TH (makeRefineType)


-- | Location of a 'Contribution' in a 'VDocVersion'.  If the begin point (resp. end point) is
-- 'Nothing', the 'ChunkRange' starts at the beginning (resp. end) of the 'VDocVersion'.  When the
-- 'Contribution' is created, it must be 'assert'ed that @0 <= begin < end < length of
-- 'VDocVersion'@.
--
-- TODO: this is called 'SelectionState' now.
data ChunkRange = ChunkRange
  { _chunkRangeBegin   :: Maybe ChunkPoint
  , _chunkRangeEnd     :: Maybe ChunkPoint
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | A point in a 'VDocVersion' in state 'HTMLCanonical' or 'HTMLWithMarks' (either begin or end) as
-- returned by `window.getSelection()` in javascript.  The 'DataUID' points to a node of the form
-- @Node _ [ContentText _]@.  Begin and end points form a 'ChunkRange'.
--
-- TODO: this is called 'SelectionPoint' now.
data ChunkPoint = ChunkPoint
  { _chunkPointNode :: DataUID
  , _chunkPointOffset :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Identifier to connect dom content with 'VDocVersion' subtrees.  We need this for interpreting
-- `getSelection()` values on 'VDocVersion's.  A dom-node with attribute @data-uid@ can be
-- identified with a root node of a subtree in 'VDocVersion' with the same @data-uid@ value.  Not to
-- be confused with 'DataContributionID'.
--
-- TODO: this is called EntityKey now.
newtype DataUID = DataUID { unDataUID :: Int }
  deriving (Eq, Ord, Generic, Num)

instance Show DataUID where  -- FIXME: derive Show and use 'toUrlPiece' for rendering.
  showsPrec n (DataUID i) = showsPrec n i

instance Read DataUID where  -- FIXME: derive Read and use 'fromUrlPiece' parsing.
  readsPrec n = (_1 %~ DataUID) <$$> readsPrec n


-- * instances

makeRefineType ''ChunkRange
type instance Create ChunkRange = ChunkRange

instance SOP.Generic ChunkPoint
instance SOP.HasDatatypeInfo ChunkPoint
instance NFData   ChunkPoint where rnf       = SOP.grnf
instance ToJSON   ChunkPoint where
  toJSON (ChunkPoint node offset) = object ["node" .= node, "offset" .= offset]
instance FromJSON ChunkPoint where
  parseJSON = withObject "ChunkPoint" (\v -> ChunkPoint <$> v .: "node" <*> v .: "offset")


makeRefineType ''DataUID
