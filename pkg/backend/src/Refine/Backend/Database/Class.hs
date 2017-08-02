{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Refine.Backend.Database.Class where

import Refine.Backend.Prelude

import qualified Web.Users.Persistent as Users

import           Refine.Backend.Database.Types
import           Refine.Common.Types
import qualified Refine.Common.OT as OT


type MonadDatabase db = (Monad db, Database db)

class Database db where

  -- VDoc
  listVDocs          :: db [ID VDoc]
  createVDoc         :: CreateVDoc -> db VDoc
  updateVDoc         :: ID VDoc -> VDoc -> db ()
  getVDoc            :: ID VDoc -> db VDoc
  vdocOfEdit         :: ID Edit -> db (ID VDoc)
  moveVDocHead       :: ID VDoc -> ID Edit -> db ()

  -- Repo
  getEditIDs         :: ID VDoc -> db [ID Edit]

  -- Edit
  createEdit         :: ID VDoc -> EditSource (ID Edit) -> CreateEdit -> db Edit
  getEdit            :: ID Edit -> db Edit
  getVersion         :: ID Edit -> db RawContent
  editNotes          :: ID Edit -> db [ID Note]
  editQuestions      :: ID Edit -> db [ID Question]
  editDiscussions    :: ID Edit -> db [ID Discussion]
  updateVotes        :: ID Edit -> (Votes -> Votes) -> db ()
  getVoteCount       :: ID Edit -> db VoteCount
  getEditChildren    :: ID Edit -> db [ID Edit]
  updateEdit         :: ID Edit -> CreateEdit -> db ()
  updateEditSource   :: ID Edit -> (ID Edit{-parent-} -> OT.Edit RawContent -> OT.Edit RawContent) -> db ()

  -- Note
  createNote         :: ID Edit -> CreateNote -> db Note
  getNote            :: ID Note -> db Note

  -- Question
  createQuestion     :: ID Edit     -> CreateQuestion -> db Question
  getQuestion        :: ID Question -> db Question

  -- Answer
  createAnswer       :: ID Question -> CreateAnswer -> db Answer
  getAnswer          :: ID Answer   -> db Answer
  answersOfQuestion  :: ID Question -> db [Answer]

  -- Discussion
  createDiscussion   :: ID Edit    -> CreateDiscussion -> db Discussion
  rebaseDiscussion   :: ID Edit -> ID Discussion -> (Range Position -> Range Position) -> db Discussion
  getDiscussion      :: ID Discussion -> db Discussion
  statementsOfDiscussion :: ID Discussion -> db [ID Statement]
  discussionOfStatement  :: ID Statement  -> db (ID Discussion)

  -- Statement
  createStatement      :: ID Statement -> CreateStatement -> db Statement
  getStatement         :: ID Statement -> db Statement

  -- User
  runUsersCmd          :: (Users.Persistent -> IO a) -> db a

  -- Group
  createGroup          :: CreateGroup -> db Group
  getGroup             :: ID Group -> db Group
  getGroups            :: db [Group]
  modifyGroup          :: ID Group -> CreateGroup -> db Group
  removeGroup          :: ID Group -> db ()
  addSubGroup          :: ID Group -> ID Group -> db ()
  removeSubGroup       :: ID Group -> ID Group -> db ()

  -- Roles
  assignRole   :: ID Group -> ID User -> Role -> db ()
  getRoles     :: ID Group -> ID User -> db [Role]
  unassignRole :: ID Group -> ID User -> Role -> db ()

  createMetaID_ :: HasMetaInfo a => ID a -> db (MetaID a)
  getMetaID     :: HasMetaInfo a => ID a -> db (MetaID a)


-- * composite db queries

compositeQuestion
  :: (Monad db, Database db)
  => ID Question -> db CompositeQuestion
compositeQuestion qid =
  CompositeQuestion <$> getQuestion qid <*> answersOfQuestion qid

editComments
  :: (Monad db, Database db)
  => ID Edit -> db [Comment]
editComments pid = do
  notes       <- mapM getNote =<< editNotes pid
  questions   <- mapM Refine.Backend.Database.Class.compositeQuestion =<< editQuestions pid
  discussions <- mapM getDiscussion =<< editDiscussions pid
  pure $ concat
    [ CommentNote       <$> notes
    , CommentQuestion   <$> questions
    , CommentDiscussion <$> discussions
    ]
