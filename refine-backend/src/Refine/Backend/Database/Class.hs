module Refine.Backend.Database.Class where

import Refine.Backend.DocRepo.Core as DocRepo
import Refine.Common.Types.Note
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc


class Database db where

  -- * VDoc
  listVDocs      :: db [ID VDoc]
  createVDoc     :: Create VDoc -> VDocRepo -> db VDoc
  getVDoc        :: ID VDoc -> db VDoc
  vdocRepo       :: ID VDoc -> db (ID VDocRepo)

  -- * Repo
  createRepo     :: DocRepo.RepoHandle -> ID Patch -> db VDocRepo
  getRepo        :: ID VDocRepo -> db VDocRepo
  getRepoHandle  :: ID VDocRepo -> db DocRepo.RepoHandle
  repoPatches    :: ID VDocRepo -> db [ID Patch]

  -- * Patch
  createPatch    :: DocRepo.PatchHandle -> db Patch
  getPatch       :: ID Patch -> db Patch
  getPatchHandle :: ID Patch -> db DocRepo.PatchHandle
  patchComments  :: ID Patch -> db [ID Comment]
  patchNotes     :: ID Patch -> db [ID Note]

  -- * Comment
  getComment     :: ID Comment -> db Comment

  -- * Note
  getNote        :: ID Note -> db Note
