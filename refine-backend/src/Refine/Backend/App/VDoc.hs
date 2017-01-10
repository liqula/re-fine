module Refine.Backend.App.VDoc where

import Control.Lens ((^.))
import Refine.Common.Patch
import Refine.Common.Prelude
import Refine.Common.VDoc
import Refine.Backend.App.Core
import Refine.Backend.Database (DB)
import Refine.Backend.Database.Entity
import qualified Refine.Backend.DocRepo as DocRepo



addVDoc :: Proto VDoc -> App DB VDoc
addVDoc pv = do
  appLog "addVDoc"
  (dr, dp) <- docRepo $ do
    dr <- DocRepo.createRepo
    dp <- DocRepo.createPatch dr (error "where do i get the parent patch id here?") (pv ^. protoVDocVersion)
    pure (dr, dp)
  db $ do
    p <- createPatch dp
    r <- createRepo dr (p ^. patchId)
    createVDoc pv r

getVDoc :: ID VDoc -> App DB VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ loadVDoc i
