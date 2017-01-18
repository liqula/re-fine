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
import           Refine.Common.Rest (CompositeVDoc(..))
import           Refine.Common.Types.Prelude
import           Refine.Common.Types.VDoc
import           Refine.Common.Types.Chunk
import           Refine.Prelude ((<@>))


listVDocs :: App DB [VDoc]
listVDocs = do
  appLog "listVDocs"
  db $ mapM DB.getVDoc =<< DB.listVDocs

createCompositeVDoc :: Create VDoc -> App DB CompositeVDoc
createCompositeVDoc = (getCompositeVDoc . view vdocID) <=< createVDoc

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

getCompositeVDoc :: ID VDoc -> App DB CompositeVDoc
getCompositeVDoc vid = do
  appLog "getCompositeVDoc"
  join . db $ do
    vdoc     <- DB.getVDoc vid
    rid      <- DB.vdocRepo vid
    rhandle  <- DB.getRepoHandle rid
    headid   <- view vdocHeadPatch <$> DB.getRepo rid
    hhandle  <- DB.getPatchHandle headid
    pids     <- DB.getPatchIDs rid
    patches  <- mapM DB.getPatch pids                                        -- TODO: only child patches to head!
    comments <- mapM DB.getComment . mconcat =<< mapM DB.patchComments pids  -- TODO only comments applicable to head!
    notes    <- mapM DB.getNote    . mconcat =<< mapM DB.patchNotes    pids  -- TODO only notes applicable to head!

    let chunkranges = undefined
          -- (view patchRange <$> patches) <>
          -- (view commentRange <$> comments) <>
          -- (view noteRange <$> notes)

        -- for now, just do comments, and leave everything else for later.

        -- TODO: this list of chunk ranges is needed for renderVDocHtml to work, but it's tricky:
        -- ChunkRange takes an type parameter that identifies what it belongs to.  we need to
        -- somehow bind the chunk range owner differently to be able to put all those different
        -- chunk ranges into one list.  i think we should try @data ChunkRange = { ..  label ::
        -- forall a . ID a .. }@.

    pure $ do
      CompositeVDoc vdoc
        <$> (renderVDocHtml chunkranges <$> docRepo (DocRepo.getVersion rhandle hhandle))
        <@> patches
        <@> comments
        <@> notes

-- TODO: implement this using 'Refine.Common.VDoc.HTML.Splice.insertMarks' from MR!12.
renderVDocHtml :: [ChunkRange a] -> VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
renderVDocHtml _ (VDocVersion v) = VDocVersion v


getVersion :: ID Patch -> App DB (VDocVersion 'HTMLWithMarks)
getVersion _ = do
  pure $ VDocVersion "Refine.Backend.App.VDoc.getVersion"
