module Refine.Backend.DocRepo.Darcs where

import Darcs.Patch()
import Refine.Backend.DocRepo.Core
import Refine.Common.VDoc


createRepo :: DocRepo Repository
createRepo = pure $ Repository "repo" "repo-1"

createPatch :: Repository -> PatchID -> Version -> DocRepo Patch
createPatch _repo _base _vers = pure $ Patch "ffffff"

createInitialPatch :: Repository -> Version -> DocRepo Patch
createInitialPatch _repo _vers = pure $ Patch "ffffff"

getVersion :: RepoID -> PatchID -> DocRepo Version
getVersion = undefined
