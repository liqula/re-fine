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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Chunk where

import           Data.Functor.Infix ((<$$>))
import           Control.DeepSeq
import           Control.Lens (makeLenses, makePrisms, _1, (%~))
import           Data.Aeson
import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP
import           GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude (ClearTypeParameter(..))
import Refine.Prelude.Generic
import Refine.Prelude.TH (makeRefineType)


-- | Location of a 'Edit', 'Comment', ... in a 'VDocVersion'.  'Edit' etc. are called the *owner*
-- of the 'ChunkRange'.  If the begin point (resp. end point) is 'Nothing', the 'ChunkRange' starts
-- at the beginning (resp. end) of the 'VDocVersion'.  When the owner is created, it must be
-- 'assert'ed that @0 <= begin < end < length of 'VDocVersion'@.
data ChunkRange owner = ChunkRange
  { _chunkRangeLabel :: ID owner
  , _chunkRangeBegin :: Maybe ChunkPoint
  , _chunkRangeEnd   :: Maybe ChunkPoint
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateChunkRange = CreateChunkRange
  { _createChunkRangeBegin :: Maybe ChunkPoint
  , _createChunkRangeEnd   :: Maybe ChunkPoint
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | A point in a 'VDocVersion' in state 'HTMLCanonical' or 'HTMLWithMarks' (either begin or end) as
-- returned by `window.getSelection()` in javascript.  The 'DataUID' points to a node of the form
-- @Node _ [ContentText _]@.  Begin and end points form a 'ChunkRange'.
data ChunkPoint = ChunkPoint
  { _chunkPointNode :: DataUID
  , _chunkPointOffset :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Identifier to connect dom content with 'VDocVersion' subtrees.  A dom-node with attribute
-- @data-uid@ can be identified with a root node of a subtree in 'VDocVersion' with the same
-- @data-uid@ value.  Not to be confused with 'DataChunkID'.
newtype DataUID = DataUID { unDataUID :: Int }
  deriving (Eq, Ord, Generic, Num)

instance Show DataUID where
  showsPrec n (DataUID i) = showsPrec n i

instance Read DataUID where
  readsPrec n = (_1 %~ DataUID) <$$> readsPrec n

-- * clear type param

instance ClearTypeParameter ChunkRange where
  clearTypeParameter (ChunkRange i b e) = ChunkRange (clearTypeParameter i) b e

-- * instances

makeLenses ''ChunkRange
makePrisms ''ChunkRange

-- FIXME: use 'makeRefineType' once that can handle parametric types.
instance SOP.Generic (ChunkRange owner)
instance SOP.HasDatatypeInfo (ChunkRange owner)
instance ToJSON   (ChunkRange owner) where toJSON    = gtoJSONDef  -- TODO: encode owner in json object?
instance FromJSON (ChunkRange owner) where parseJSON = gparseJSONDef
instance NFData   (ChunkRange owner) where rnf       = SOP.grnf

type instance Create (ChunkRange owner) = CreateChunkRange

instance SOP.Generic ChunkPoint
instance SOP.HasDatatypeInfo ChunkPoint
instance NFData   ChunkPoint where rnf       = SOP.grnf
instance ToJSON   ChunkPoint where
  toJSON (ChunkPoint node offset) = object ["node" .= node, "offset" .= offset]
instance FromJSON ChunkPoint where
  parseJSON = withObject "ChunkPoint" (\v -> ChunkPoint <$> v .: "node" <*> v .: "offset")


makeRefineType ''CreateChunkRange
makeRefineType ''DataUID
