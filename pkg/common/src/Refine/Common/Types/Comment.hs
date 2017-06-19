{-# LANGUAGE NoImplicitPrelude          #-}
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

module Refine.Common.Types.Comment where

import Refine.Common.Prelude

import Control.Lens (Lens')
import Data.String.Conversions (ST)
import Data.Tree (Tree)
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Common.Types.Position


type CommentText = ST  -- FIXME: refactor VDocVersion to be more general and use that.

data CreateNote = CreateNote
  { _createNoteText   :: CommentText
  , _createNotePublic :: Bool
  , _createNoteRange  :: Range Position
  }
  deriving (Eq, Ord, Show, Generic)

data Note = Note
  { _noteMetaID :: MetaID Note
  , _noteText   :: CommentText
  , _notePublic :: Bool
  , _noteRange  :: Range Position
  }
  deriving (Eq, Ord, Show, Generic)

data CreateQuestion = CreateQuestion
  { _createQuestionText   :: ST
  , _createQuestionPublic :: Bool
  , _createQuestionRange  :: Range Position
  }
  deriving (Eq, Ord, Show, Generic)

data Question = Question
  { _questionMetaID   :: MetaID Question
  , _questionText     :: ST
  , _questionAnswered :: Bool -- ^ if the asker is happy, she can mark it as answered.
  , _questionPublic   :: Bool
  , _questionRange    :: Range Position
  }
  deriving (Eq, Ord, Show, Generic)

data CompositeQuestion = CompositeQuestion
  { _compositeQuestion        :: Question
  , _compositeQuestionAnswers :: [Answer]
  }
  deriving (Eq, Ord, Show, Generic)

newtype CreateAnswer = CreateAnswer
  { _createAnswerText :: CommentText
  }
  deriving (Eq, Ord, Show, Generic)

data Answer = Answer
  { _answerMetaID   :: MetaID Answer
  , _answerQuestion :: ID Question
  , _answerText     :: CommentText
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateDiscussion = CreateDiscussion
  { _createDiscussionStatementText :: CommentText
  , _createDiscussionPublic        :: Bool
  , _createDiscussionRange         :: Range Position
  }
  deriving (Eq, Ord, Show, Generic)

data Discussion = Discussion
  { _discussionMetaID :: MetaID Discussion
  , _discussionPublic :: Bool
  , _discussionRange  :: Range Position
  }
  deriving (Eq, Ord, Show, Generic)

data CompositeDiscussion = CompositeDiscussion
  { _compositeDiscussion     :: Discussion
  , _compositeDiscussionTree :: Tree Statement
  }
  deriving (Eq, Show, Generic)

newtype CreateStatement = CreateStatement
  { _createStatementText :: CommentText
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Statement = Statement
  { _statementMetaID :: MetaID Statement
  , _statementText   :: CommentText
  , _statementParent :: Maybe (ID Statement)
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Comment =
    CommentNote Note
  | CommentDiscussion CompositeDiscussion
  | CommentQuestion CompositeQuestion
  deriving (Eq, Show, Generic)

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

-- * Lenses

noteID :: Lens' Note (ID Note)
noteID = noteMetaID . miID

questionID :: Lens' Question (ID Question)
questionID = questionMetaID . miID

answerID :: Lens' Answer (ID Answer)
answerID = answerMetaID . miID

discussionID :: Lens' Discussion (ID Discussion)
discussionID = discussionMetaID . miID

statementID :: Lens' Statement (ID Statement)
statementID = statementMetaID . miID
