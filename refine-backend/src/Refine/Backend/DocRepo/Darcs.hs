module Refine.Backend.DocRepo.Darcs where

import Darcs.Patch()
import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc


createRepo :: DocRepo RepoHandle
createRepo = undefined

createPatch :: RepoHandle -> PatchHandle -> VDocVersion -> DocRepo PatchHandle
createPatch _repo _base _vers = undefined

createInitialPatch :: RepoHandle -> VDocVersion -> DocRepo PatchHandle
createInitialPatch _repo _vers = undefined

getVersion :: RepoHandle -> PatchHandle -> DocRepo VDocVersion
getVersion = undefined
