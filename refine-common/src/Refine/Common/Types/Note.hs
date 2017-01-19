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

module Refine.Common.Types.Note where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Chunk
import Refine.Common.Types.Prelude
import Refine.Prelude.TH


-- | TODO: re-think the types in this module before proceeding with the implementation.

-- | The 'ChunkRange' here is coming from the user.  It refers to the selection (javascript
-- `window.getSelection()`) that the user made before entering the comment.  'Nothing' means comment
-- refers to the entire document.
data Comment = Comment
  { _commentID     :: ID Comment
  , _commentText   :: ST
  , _commentPublic :: Bool
  , _commentRange  :: ChunkRange Comment
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Note = Note
  { _noteID    :: ID Note
  , _noteText  :: ST
  , _noteKind  :: NoteKind
  , _noteRange :: ChunkRange Note
  }
  deriving (Eq, Ord, Show, Read, Generic)

data NoteKind = Question | Remark
  deriving (Eq, Ord, Show, Read, Generic)

data CreateComment = CreateComment
  { _createCommentText   :: ST
  , _createCommentPublic :: Bool
  , _createCommentRange  :: CreateChunkRange
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateNote = CreateNote
  deriving (Eq, Ord, Show, Read, Generic)


-- * create types

type instance Create Comment = CreateComment
type instance Create Note    = CreateNote


-- * refine types

makeRefineType ''Comment
makeRefineType ''Note
makeRefineType ''NoteKind
makeRefineType ''CreateComment
makeRefineType ''CreateNote
