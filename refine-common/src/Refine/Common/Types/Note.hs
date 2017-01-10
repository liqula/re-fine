module Refine.Common.Types.Note where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Chunk
import Refine.Common.Types.Prelude
import Refine.Prelude.TH



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

-- * refine types

makeRefineType ''Comment
makeRefineType ''Note
makeRefineType ''NoteKind
makeRefineType ''ProtoComment
makeRefineType ''ProtoNote
