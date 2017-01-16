{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.App.VDoc where

import Control.Lens ((^.), view)
import Control.Monad ((<=<), join, mapM)

import           Refine.Backend.App.Core
import           Refine.Backend.Database (DB)
import qualified Refine.Backend.Database.Class as DB
import qualified Refine.Backend.DocRepo as DocRepo
import           Refine.Common.Rest (HeavyVDoc(..))
import           Refine.Common.Types.Prelude
import           Refine.Common.Types.VDoc
import           Refine.Prelude ((<@>))


listVDocs :: App DB [ID VDoc]
listVDocs = do
  appLog "listVDocs"
  db DB.listVDocs

createHeavyVDoc :: Create VDoc -> App DB HeavyVDoc
createHeavyVDoc = (getHeavyVDoc . view vdocID) <=< createVDoc

createVDoc :: Create VDoc -> App DB VDoc
createVDoc pv = do
  appLog "createVDoc"
  (dr, dp) <- docRepo $ do
    dr <- DocRepo.createRepo
    dp <- DocRepo.createInitialPatch dr (pv ^. protoVDocInitVersion)
    pure (dr, dp)
  db $ do
    p <- DB.createPatch dp
    r <- DB.createRepo dr (p ^. patchId)
    DB.createVDoc pv r

getVDoc :: ID VDoc -> App DB VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ DB.getVDoc i

getHeavyVDoc :: ID VDoc -> App DB HeavyVDoc
getHeavyVDoc vid = do
  appLog "getHeavyVDoc"
  join . db $ do
    vdoc     <- DB.getVDoc vid
    rid      <- DB.vdocRepo vid
    rhandle  <- DB.getRepoHandle rid
    headid   <- view vdocHeadPatch <$> DB.getRepo rid
    hhandle  <- DB.getPatchHandle headid
    pids     <- DB.repoPatches rid
    patches  <- mapM DB.getPatch pids
    comments <- mapM DB.getComment . mconcat =<< mapM DB.patchComments pids
    notes    <- mapM DB.getNote    . mconcat =<< mapM DB.patchNotes    pids
    pure $ do
      HeavyVDoc vdoc
        <$> (renderVDocHtml <$> docRepo (DocRepo.getVersion rhandle hhandle))
        <@> patches
        <@> comments
        <@> notes

-- TODO: Implement it, and not just relabel the content.
renderVDocHtml :: VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
renderVDocHtml (VDocVersion v) = VDocVersion v

getVersion :: ID Patch -> App DB (VDocVersion 'HTMLWithMarks)
getVersion _ = do
  pure $ VDocVersion "Refine.Backend.App.VDoc.getVersion"
