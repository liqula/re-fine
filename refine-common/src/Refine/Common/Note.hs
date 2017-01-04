module Refine.Common.Note where

import Control.DeepSeq
import Control.Lens
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Chunk
import Refine.Common.Prelude

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP



data ChangeNoteVisibility
  = ChangeCommentVisibility
    { _changeVisibilityCommentKey  :: ID Comment
    , _changeNoteVisibilityVisible :: Bool
    }
  | ChangeNoteVisibility
    { _changeVisibilityNoteKey     :: ID Note
    , _changeNoteVisibilityVisible :: Bool
    }
  deriving (Eq, Ord, Show, Read, Generic)

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

makeLenses ''ChangeNoteVisibility
makeLenses ''Comment
makeLenses ''Note
makeLenses ''NoteKind

makePrisms ''ChangeNoteVisibility
makePrisms ''Comment
makePrisms ''Note
makePrisms ''NoteKind

-- * SOP

instance SOP.Generic ChangeNoteVisibility
instance SOP.Generic Comment
instance SOP.Generic Note
instance SOP.Generic NoteKind

-- * NFData

instance NFData ChangeNoteVisibility where rnf = SOP.grnf
instance NFData Comment where rnf = SOP.grnf
instance NFData Note where rnf = SOP.grnf
instance NFData NoteKind where rnf = SOP.grnf
