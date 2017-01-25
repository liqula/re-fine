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

module Refine.Common.Types.Note where  -- rename to Comment

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Chunk
import Refine.Common.Types.Prelude
import Refine.Prelude.TH


type CommentText = ST  -- FIXME: refactor VDocVersion to be more general and use that.

data CreateNote = CreateNote
  { _createNoteText   :: CommentText
  , _createNotePublic :: Bool
  , _createNoteRange  :: CreateChunkRange
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Note = Note
  { _noteID         :: ID Note
  , _noteText       :: CommentText
  , _notePublic     :: Bool
  , _noteChunkRange :: ChunkRange Note
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateQuestion = CreateQuestion
  { _createQuestionText   :: ST
  , _createQuestionPublic :: Bool
  , _createQuestionRange  :: CreateChunkRange
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Question = Question
  { _questionID       :: ID Question
  , _questionText     :: ST
  , _questionAnswered :: Bool -- ^ if the asker is happy, she can mark it as answered.
  , _questionPublic   :: Bool
  , _questionChunkRange :: ChunkRange Question
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CompositeQuestion = CompositeQuestion
  { _compositeQuestion        :: Question
  , _compositeQuestionAnswers :: [Answer]
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype CreateAnswer = CreateAnswer
  { _createAnswerText :: CommentText
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Answer = Answer
  { _answerID       :: ID Answer
  , _answerQuestion :: ID Question
  , _answerText     :: CommentText
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateDiscussion = CreateDiscussion
  { _createDiscussionPublic :: Bool
  , _createDiscussionRange  :: CreateChunkRange
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Discussion = Discussion
  { _discussionID         :: ID Discussion
  , _discussionPublic     :: Bool
  , _discussionChunkRange :: ChunkRange Discussion
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CompositeDiscussion = CompositeDiscussion
  { _compositeDiscussion     :: Discussion
  , _compositeDiscussionTree :: [Statement] -- FIXME: This should be a tree.
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype CreateStatement = CreateStatement
  { _createStatementText :: CommentText
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Statement = Statement
  { _statementID     :: ID Statement
  , _statementText   :: CommentText
  , _statementParent :: Maybe (ID Statement)
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Comment =
    CommentNote Note
  | CommentDiscussion CompositeDiscussion
  | CommentQuestion CompositeQuestion
  deriving (Eq, Ord, Show, Read, Generic)

-- * create types

type instance Create Note       = CreateNote
type instance Create Question   = CreateQuestion
type instance Create Answer     = CreateAnswer
type instance Create Discussion = CreateDiscussion
type instance Create Statement  = CreateStatement

-- * Refine types

makeRefineType ''CreateNote
makeRefineType ''Note
makeRefineType ''CreateQuestion
makeRefineType ''Question
makeRefineType ''CompositeQuestion
makeRefineType ''CreateAnswer
makeRefineType ''Answer
makeRefineType ''CreateDiscussion
makeRefineType ''Discussion
makeRefineType ''CompositeDiscussion
makeRefineType ''CreateStatement
makeRefineType ''Statement
makeRefineType ''Comment
