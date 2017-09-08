{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Database.Class where
#include "import_backend.hs"

import qualified Web.Users.Persistent as Users

import           Refine.Backend.Database.Types
import           Refine.Backend.Types
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
  editDiscussions    :: ID Edit -> db [(ID Discussion, Range Position)]
  updateVotes        :: ID Edit -> (Votes -> Votes) -> db ()
  getVoteCount       :: ID Edit -> db VoteCount
  getEditChildren    :: ID Edit -> db [ID Edit]
  updateEdit         :: ID Edit -> CreateEdit -> db ()
  updateEditSource   :: ID Edit -> (ID Edit{-parent-} -> OT.Edit RawContent -> OT.Edit RawContent) -> db ()

  -- Discussion
  createDiscussion   :: ID Edit -> CreateDiscussion (Range Position) -> db Discussion
  rebaseDiscussion   :: ID Edit -> ID Edit -> ID Discussion -> (Range Position -> Range Position) -> db Discussion
  getDiscussion      :: ID Discussion -> db Discussion
  statementsOfDiscussion :: ID Discussion -> db [ID Statement]
  discussionOfStatement  :: ID Statement  -> db (ID Discussion)
  updateDiscussionVotes  :: ID Discussion -> (Votes -> Votes) -> db ()

  -- Statement
  createStatement      :: ID Statement -> CreateStatement -> db Statement
  updateStatement      :: ID Statement -> CreateStatement -> db Statement
  getStatement         :: ID Statement -> db Statement

  -- User
  runUsersCmd          :: (Users.Persistent -> IO a) -> db a
  insertDBUser         :: ID User -> UserDetails -> db ()
  replaceDBUser        :: ID User -> UserDetails -> db ()
  getDBUser            :: ID User -> db UserDetails

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
  => ID Edit -> db [(Discussion, Range Position)]
editComments pid = mapM (\(i, r) -> flip (,) r <$> getDiscussion i) =<< editDiscussions pid
