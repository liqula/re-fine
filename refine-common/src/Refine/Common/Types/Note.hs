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

data Comment = Comment
  { _commentID     :: ID Comment
  , _commentText   :: ST
  , _commentPublic :: Bool
  , _commentRange  :: ChunkRange Comment
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Note = Note
  deriving (Eq, Ord, Show, Read, Generic)

data NoteKind = Question | Remark
  deriving (Eq, Ord, Show, Read, Generic)

-- suggestion: Rename this to CommentRequest? (and the others as well ofc)
data CreateComment = CreateComment
  deriving (Eq, Ord, Show, Read, Generic)

data CreateNote = CreateNote
  deriving (Eq, Ord, Show, Read, Generic)


-- * prototype

type instance Create Comment = CreateComment
type instance Create Note    = CreateNote


-- * refine types

makeRefineType ''Comment
makeRefineType ''Note
makeRefineType ''NoteKind
makeRefineType ''CreateComment
makeRefineType ''CreateNote
