module Refine.Common.Note where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Chunk
import Refine.Common.Prelude

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP



data Comment = Comment
  { _commentID     :: ID Comment
  , _commentText   :: ST
  , _commentPublic :: Bool
  , _commentRange  :: Maybe ChunkRange
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Note = Note
  deriving (Eq, Ord, Show, Read, Generic)

data NoteKind = Question | Remark
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoComment = ProtoComment
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoNote = ProtoNote
  deriving (Eq, Ord, Show, Read, Generic)

-- * prototype

type instance Proto Comment = ProtoComment
type instance Proto Note    = ProtoNote

-- * lens

makeLenses ''Comment
makeLenses ''Note
makeLenses ''NoteKind
makeLenses ''ProtoComment
makeLenses ''ProtoNote

makePrisms ''Comment
makePrisms ''Note
makePrisms ''NoteKind
makePrisms ''ProtoComment
makePrisms ''ProtoNote

-- * SOP

instance SOP.Generic Comment
instance SOP.Generic Note
instance SOP.Generic NoteKind
instance SOP.Generic ProtoComment
instance SOP.Generic ProtoNote

instance SOP.HasDatatypeInfo Comment
instance SOP.HasDatatypeInfo Note
instance SOP.HasDatatypeInfo NoteKind
instance SOP.HasDatatypeInfo ProtoComment
instance SOP.HasDatatypeInfo ProtoNote

-- * NFData

instance NFData Comment where rnf = SOP.grnf
instance NFData Note where rnf = SOP.grnf
instance NFData NoteKind where rnf = SOP.grnf
instance NFData ProtoComment where rnf = SOP.grnf
instance NFData ProtoNote where rnf = SOP.grnf

-- * JSON

instance ToJSON Comment where toJSON = gtoJSONDef
instance ToJSON Note where toJSON = gtoJSONDef
instance ToJSON NoteKind where toJSON = gtoJSONDef
instance ToJSON ProtoComment where toJSON = gtoJSONDef
instance ToJSON ProtoNote where toJSON = gtoJSONDef

instance FromJSON Comment where parseJSON = gparseJSONDef
instance FromJSON Note where parseJSON = gparseJSONDef
instance FromJSON NoteKind where parseJSON = gparseJSONDef
instance FromJSON ProtoComment where parseJSON = gparseJSONDef
instance FromJSON ProtoNote where parseJSON = gparseJSONDef
