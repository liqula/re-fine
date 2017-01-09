module Refine.Backend.App.VDoc where

import Refine.Common.Prelude
import Refine.Common.VDoc
import Refine.Backend.App.Core
import Refine.Backend.Database (DB)
import Refine.Backend.Database.Entity
import qualified Refine.Backend.DocRepo as DocRepo



addVDoc :: Proto VDoc -> App DB VDoc
addVDoc pv = do
  appLog "addVDoc"
  let value = undefined
  (dr, dp) <- docRepo $ do
    dr <- DocRepo.create
    dp <- DocRepo.commit dr value
    pure (dr, dp)
  db $ do
    r <- createRepo dr
    p <- createPatch r dp
    createVDoc pv r p

getVDoc :: ID VDoc -> App DB VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ loadVDoc i
