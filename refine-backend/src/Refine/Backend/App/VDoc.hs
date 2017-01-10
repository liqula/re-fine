{-# LANGUAGE OverloadedStrings #-}

module Refine.Backend.App.VDoc where

import Control.Lens ((^.))
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc
import Refine.Backend.App.Core
import Refine.Backend.Database (DB)
import Refine.Backend.Database.Entity as DB
import qualified Refine.Backend.DocRepo as DocRepo



createVDoc :: Proto VDoc -> App DB VDoc
createVDoc pv = do
  appLog "createVDoc"
  (dr, dp) <- docRepo $ do
    dr <- DocRepo.createRepo
    dp <- DocRepo.createInitialPatch dr (pv ^. protoVDocInitVersion)
    pure (dr, dp)
  db $ do
    p <- createPatch dp
    r <- createRepo dr (p ^. patchId)
    DB.createVDoc pv r

getVDoc :: ID VDoc -> App DB VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ loadVDoc i

getVersion :: ID Patch -> App DB VDocVersion
getVersion _ = do
  pure $ VDocVersion "Refine.Backend.App.VDoc.getVersion"
