{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.Database.Class where

import Refine.Backend.Prelude

import qualified Web.Users.Persistent as Users

import           Refine.Backend.Database.Types
import           Refine.Common.Types
import qualified Refine.Common.OT as OT


class Monad db => Database db where

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
  editDiscussions    :: ID Edit -> db [ID Discussion]
  updateVotes        :: ID Edit -> (Votes -> Votes) -> db ()
  getVoteCount       :: ID Edit -> db VoteCount
  getEditChildren    :: ID Edit -> db [ID Edit]
  updateEdit         :: ID Edit -> CreateEdit -> db ()
  updateEditSource   :: ID Edit -> (ID Edit{-parent-} -> OT.Edit RawContent -> OT.Edit RawContent) -> db ()

  -- Note
  createNote         :: ID Edit -> CreateNote (Range Position) -> db Note
  getNote            :: ID Note -> db Note
  updateNoteVotes    :: ID Note -> (Votes -> Votes) -> db ()

  -- Discussion
  createDiscussion   :: ID Edit -> CreateDiscussion (Range Position) -> db Discussion
  rebaseDiscussion   :: ID Edit -> ID Discussion -> (Range Position -> Range Position) -> db Discussion
  getDiscussion      :: ID Discussion -> db Discussion
  statementsOfDiscussion :: ID Discussion -> db [ID Statement]
  discussionOfStatement  :: ID Statement  -> db (ID Discussion)

  -- Statement
  createStatement      :: ID Statement -> CreateStatement -> db Statement
  updateStatement      :: ID Statement -> CreateStatement -> db Statement
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
  assignGroupRole   :: ID Group -> ID User -> GroupRole -> db ()
  getGroupRolesIn   :: ID Group -> ID User -> db [GroupRole]
  getGroupRoles     :: ID User -> db [(GroupRole, ID Group)]
  unassignGroupRole :: ID Group -> ID User -> GroupRole -> db ()

  assignGlobalRole   :: ID User -> GlobalRole -> db ()
  getGlobalRoles     :: ID User -> db [GlobalRole]
  unassignGlobalRole :: ID User -> GlobalRole -> db ()

  -- MetaInfo
  createMetaID_ :: HasMetaInfo a => ID a -> db (MetaID a)
  getMetaID     :: HasMetaInfo a => ID a -> db (MetaID a)


-- * composite db queries

editComments
  :: (Monad db, Database db)
  => ID Edit -> db [Comment]
editComments pid = do
  notes       <- mapM getNote =<< editNotes pid
  discussions <- mapM getDiscussion =<< editDiscussions pid
  pure $ mconcat
    [ CommentNote       <$> notes
    , CommentDiscussion <$> discussions
    ]
