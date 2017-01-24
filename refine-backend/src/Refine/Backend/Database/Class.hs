module Refine.Backend.Database.Class where

import Refine.Backend.DocRepo.Core as DocRepo
import Refine.Common.Types.Note
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc


class Database db where

  -- * VDoc
  listVDocs          :: db [ID VDoc]
  createVDoc         :: Create VDoc -> VDocRepo -> db VDoc
  getVDoc            :: ID VDoc -> db VDoc
  vdocRepo           :: ID VDoc -> db (ID VDocRepo)

  -- * Repo
  createRepo         :: DocRepo.RepoHandle -> DocRepo.PatchHandle -> db VDocRepo
  getRepo            :: ID VDocRepo -> db VDocRepo
  getRepoFromHandle  :: DocRepo.RepoHandle -> db VDocRepo
  getRepoHandle      :: ID VDocRepo -> db DocRepo.RepoHandle
  getPatchIDs        :: ID VDocRepo -> db [ID Patch]

  -- * Patch
  createPatch        :: ID VDocRepo -> DocRepo.PatchHandle -> db Patch
  getPatch           :: ID Patch -> db Patch
  getPatchFromHandle :: DocRepo.PatchHandle -> db Patch
  getPatchHandle     :: ID Patch -> db DocRepo.PatchHandle
  patchComments      :: ID Patch -> db [ID Comment]
  patchNotes         :: ID Patch -> db [ID Note]

  -- * Repo and patch
  patchVDocRepo      :: ID Patch -> db (ID VDocRepo)

  -- * Comment
  createComment      :: ID Patch -> Create Comment -> db Comment
  getComment         :: ID Comment -> db Comment

  -- * Note
  createNote         :: ID Patch -> Create Note -> db Note
  getNote            :: ID Note -> db Note


-- * composite db queries

handlesForPatch
  :: (Monad db, Database db)
  => ID Patch -> db (DocRepo.RepoHandle, DocRepo.PatchHandle)
handlesForPatch pid = do
  rid <- patchVDocRepo pid
  (,) <$> getRepoHandle rid <*> getPatchHandle pid
