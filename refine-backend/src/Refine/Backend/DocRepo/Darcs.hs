module Refine.Backend.DocRepo.Darcs where

import Darcs.Patch()
import Refine.Backend.DocRepo.Core
import Refine.Common.VDoc


createRepo :: DocRepo Repository
createRepo = pure $ Repository "repo" "repo-1"

createPatch :: Repository -> Maybe PatchID -> Version -> DocRepo Patch
createPatch _repo _base _vers = pure $ Patch "ffffff"

getVersion :: RepoID -> PatchID -> DocRepo Version
getVersion = undefined
