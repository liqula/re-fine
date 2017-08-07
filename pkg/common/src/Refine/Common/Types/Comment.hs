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


type CommentText = ST

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
  } -- FIXME: add (createDiscussionEdit :: ID Edit) and simplify SAddDiscussion (fisx does not like
    -- this, but does not want to spend time arguing about it)
  deriving (Eq, Ord, Show, Generic)

data Discussion = Discussion
  { _discussionMetaID :: MetaID Discussion
  , _discussionPublic :: Bool
  , _discussionRange  :: Range Position
  , _discussionTree   :: Tree Statement
  } -- FIXME: add (discussionEdit :: ID Edit), fits better the server cache in global state
  deriving (Eq, Show, Generic)

newtype CreateStatement = CreateStatement
  { _createStatementText :: CommentText
  } -- FIXME: add (createStatementParent :: Either (ID Discussion) (ID Statement)) and remove
    -- "onstatementid" from SAddStatement (fisx does not like this, but does not want to spend time
    -- arguing about it)
  deriving (Eq, Ord, Show, Read, Generic)

data Statement = Statement
  { _statementMetaID :: MetaID Statement
  , _statementText   :: CommentText
  , _statementParent :: Maybe (ID Statement)  -- FIXME: remove this, this is unnecessary if we use the Tree data structure in Discussion
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Comment =
    CommentNote Note
  | CommentDiscussion Discussion
  | CommentQuestion CompositeQuestion
  deriving (Eq, Show, Generic)

-- * Refine types

makeRefineTypes [ ''CreateNote, ''Note, ''CreateQuestion, ''Question, ''CompositeQuestion, ''CreateAnswer
                , ''Answer, ''CreateDiscussion, ''Discussion, ''CreateStatement, ''Statement, ''Comment
                ]

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
