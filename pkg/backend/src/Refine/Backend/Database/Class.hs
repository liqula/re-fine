{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Refine.Backend.Database.Class where

import Refine.Backend.Prelude

import Data.Typeable (Typeable)
import qualified Web.Users.Persistent as Users

import           Refine.Backend.Database.Types
import           Refine.Common.Types
import qualified Refine.Common.OT as OT


type MonadDatabase db = (Monad db, Database db)

class Database db where

  -- VDoc
  listVDocs          :: db [ID VDoc]
  createVDoc         :: Create VDoc -> VDocVersion -> db VDoc
  getVDoc            :: ID VDoc -> db VDoc
  vdocOfEdit         :: ID Edit -> db (ID VDoc)
  moveVDocHead       :: ID VDoc -> ID Edit -> db ()

  -- Repo
  getEditIDs         :: ID VDoc -> db [ID Edit]

  -- Edit
  createEdit         :: ID VDoc -> EditSource (ID Edit) -> Create Edit -> db Edit
  getEdit            :: ID Edit -> db Edit
  getVersion         :: ID Edit -> db VDocVersion
  editNotes          :: ID Edit -> db [ID Note]
  editQuestions      :: ID Edit -> db [ID Question]
  editDiscussions    :: ID Edit -> db [ID Discussion]
  updateVotes        :: ID Edit -> (Votes -> Votes) -> db ()
  getVoteCount       :: ID Edit -> db VoteCount
  getEditChildren    :: ID Edit -> db [ID Edit]
  updateEdit         :: ID Edit -> Create Edit -> db ()
  updateEditSource   :: ID Edit -> (ID Edit{-parent-} -> OT.Edit RawContent -> OT.Edit RawContent) -> db ()

  -- Note
  createNote         :: ID Edit -> Create Note -> db Note
  getNote            :: ID Note -> db Note

  -- Question
  createQuestion     :: ID Edit     -> Create Question -> db Question
  getQuestion        :: ID Question -> db Question

  -- Answer
  createAnswer       :: ID Question -> Create Answer -> db Answer
  getAnswer          :: ID Answer   -> db Answer
  answersOfQuestion  :: ID Question -> db [Answer]

  -- Discussion
  createDiscussion   :: ID Edit    -> Create Discussion -> db Discussion
  rebaseDiscussion   :: ID Edit -> ID Discussion -> (Range Position -> Range Position) -> db Discussion
  getDiscussion      :: ID Discussion -> db Discussion
  statementsOfDiscussion :: ID Discussion -> db [ID Statement]
  discussionOfStatement  :: ID Statement  -> db (ID Discussion)

  -- Statement
  createStatement      :: ID Statement -> Create Statement -> db Statement
  getStatement         :: ID Statement -> db Statement

  -- User
  runUsersCmd          :: (Users.Persistent -> IO a) -> db a

  -- Group
  createGroup          :: Create Group -> db Group
  getGroup             :: ID Group -> db Group
  getGroups            :: db [Group]
  modifyGroup          :: ID Group -> Create Group -> db Group
  removeGroup          :: ID Group -> db ()
  addSubGroup          :: ID Group -> ID Group -> db ()
  removeSubGroup       :: ID Group -> ID Group -> db ()

  -- Roles
  assignRole   :: ID Group -> ID User -> Role -> db ()
  getRoles     :: ID Group -> ID User -> db [Role]
  unassignRole :: ID Group -> ID User -> Role -> db ()

  -- Process

  -- @Process (ResultDB (Process a))@ is a parametric type applied to a type family value of a
  -- polymoprhic type.  yeay!  :)
  --
  -- the reason this happens is that in 'DB', we cannot return the results in a form that they are
  -- required in by the frontend: we need 'App' to turn the @ResultDB (Process a)@ into an @a@.
  --
  -- example: we need to build the 'CompositeVDoc' (which requires "Refine.Backend.DocRepo") from
  -- the @VDoc@ that can be produced inside 'DB'.

  createProcess :: StoreProcessData db a => CreateDB (Process a) -> db (Process a)
  getProcess    :: (StoreProcessData db a, Typeable a) => ID (Process a) -> db (Process a)
  updateProcess :: StoreProcessData db a => ID (Process a) -> CreateDB (Process a) -> db ()
  removeProcess :: (StoreProcessData db a, Typeable a) => ID (Process a) -> db ()

  vDocProcess :: ID VDoc -> db (ID (Process CollaborativeEdit))

  createMetaID_ :: HasMetaInfo a => ID a -> db (MetaID a)
  getMetaID     :: HasMetaInfo a => ID a -> db (MetaID a)


class StoreProcessData db c where
  processDataGroupID :: CreateDB (Process c) -> db (ID Group)
  createProcessData  :: ID (Process c) -> CreateDB (Process c) -> db c
  getProcessData     :: ID (Process c) -> db c
  updateProcessData  :: ID (Process c) -> CreateDB (Process c) -> db ()
  removeProcessData  :: c -> db ()

class GroupOf db e where
  groupOf :: ID e -> db Group

-- | Type of contents of process, also used as an index type to identify process "kind".
type family ProcessPayload e = r

class ProcessOf db e where
  processOf :: ID e -> db (Process (ProcessPayload e))

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
