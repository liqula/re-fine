module Refine.Common.Note where

import Control.DeepSeq
import Control.Lens
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

-- * lens

makeLenses ''Comment
makeLenses ''Note
makeLenses ''NoteKind

makePrisms ''Comment
makePrisms ''Note
makePrisms ''NoteKind

-- * SOP

instance SOP.Generic Comment
instance SOP.Generic Note
instance SOP.Generic NoteKind

-- * NFData

instance NFData Comment where rnf = SOP.grnf
instance NFData Note where rnf = SOP.grnf
instance NFData NoteKind where rnf = SOP.grnf
