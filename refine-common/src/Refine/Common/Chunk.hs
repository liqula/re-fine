module Refine.Common.Chunk where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP

import Refine.Common.Prelude



data ChunkPoint = ChunkPoint
  { _chunkPointNode :: Maybe DataUID
  , _chunkPointOffset :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Location of a 'Patch', 'Comment', ... in the HTML tree of a 'VDocVersion'.  'Patch' etc. are
-- called the *owner* of the 'ChunkRange'.
--
-- ASSUMPTION: @0 <= begin < end < length of 'VDocVersion'@.
--
-- The 'ChunkRange' consists of a label and a beginning and end point.  Label is the @AUID@ of the
-- owner (or some representation thereof); beginning and end points correspond to a range object
-- provided by the browser.
data ChunkRange = ChunkRange
  { _chunkRangeLabel :: ST
  , _chunkRangeBegin :: ChunkPoint
  , _chunkRangeEnd   :: ChunkPoint
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype DataUID = DataUID { unDataUID :: Int }
  deriving (Eq, Ord, Show, Read, Generic)

-- * lens

makeLenses ''ChunkPoint
makeLenses ''ChunkRange
makeLenses ''DataUID

makePrisms ''ChunkPoint
makePrisms ''ChunkRange
makePrisms ''DataUID

-- * SOP

instance SOP.Generic ChunkPoint
instance SOP.Generic ChunkRange
instance SOP.Generic DataUID

instance SOP.HasDatatypeInfo ChunkPoint
instance SOP.HasDatatypeInfo ChunkRange
instance SOP.HasDatatypeInfo DataUID

-- * NFData

instance NFData ChunkPoint where rnf = SOP.grnf
instance NFData ChunkRange where rnf = SOP.grnf
instance NFData DataUID where rnf = SOP.grnf

-- * JSON

instance ToJSON ChunkPoint where toJSON = gtoJSONDef
instance ToJSON ChunkRange where toJSON = gtoJSONDef
instance ToJSON DataUID where toJSON = gtoJSONDef

instance FromJSON ChunkPoint where parseJSON = gparseJSONDef
instance FromJSON ChunkRange where parseJSON = gparseJSONDef
instance FromJSON DataUID where parseJSON = gparseJSONDef
