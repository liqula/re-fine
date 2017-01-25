{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

import Control.Lens ((^.), to, view)
import Control.Monad ((<=<), join, mapM)
import Data.Monoid ((<>))

import           Refine.Backend.App.Core
import           Refine.Backend.Database (DB)
import qualified Refine.Backend.Database.Class as DB
import qualified Refine.Backend.DocRepo as DocRepo
import           Refine.Common.Rest (CompositeVDoc(..))
import           Refine.Common.Types.Chunk
import           Refine.Common.Types.Comment
import           Refine.Common.Types.Prelude
import           Refine.Common.Types.VDoc
import           Refine.Common.VDoc.HTML
import           Refine.Prelude (Void, clearTP, monadError)


listVDocs :: App DB [VDoc]
listVDocs = do
  appLog "listVDocs"
  db $ mapM DB.getVDoc =<< DB.listVDocs

createVDocGetComposite :: Create VDoc -> App DB CompositeVDoc
createVDocGetComposite = (getCompositeVDoc . view vdocID) <=< createVDoc

createVDoc :: Create VDoc -> App DB VDoc
createVDoc pv = do
  appLog "createVDoc"
  vd <- pv ^. createVDocInitVersion
            . to (monadError AppVDocError . canonicalizeVDocVersion)
  (dr, dp) <- docRepo $ do
    dr <- DocRepo.createRepo
    dp <- DocRepo.createInitialPatch dr vd
    pure (dr, dp)
  db $ do
    r <- DB.createRepo dr dp
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
    repo     <- DB.getRepo rid
    let headid = repo ^. vdocHeadPatch
    hhandle  <- DB.getPatchHandle headid
    comments <- DB.patchComments headid

    let chunkRangesCN = commentChunkRange <$> comments

    pure $ do
      hpatches <- docRepo $ DocRepo.getChildPatches rhandle hhandle
      patches  <- db $ mapM DB.getPatchFromHandle hpatches
      let chunkRanges = chunkRangesCN <> (view (patchRange . to clearTP) <$> patches)
      version <- monadError AppVDocError
                 =<< insertMarks chunkRanges <$> docRepo (DocRepo.getVersion rhandle hhandle)
      pure $ CompositeVDoc vdoc repo version patches comments

  where
    commentChunkRange :: Comment -> ChunkRange Void
    commentChunkRange = \case
      CommentNote n       -> n ^. noteChunkRange . to clearTP
      CommentQuestion q   -> q ^. compositeQuestion . questionChunkRange . to clearTP
      CommentDiscussion d -> d ^. compositeDiscussion . discussionChunkRange . to clearTP

addPatch :: ID Patch -> Create Patch -> App DB Patch
addPatch basepid patch = do
  appLog "addPatch"
  join . db $ do
    rid                    <- DB.patchVDocRepo basepid
    (rhandle, basephandle) <- DB.handlesForPatch basepid
    pure $ do
      version      <- patch ^. createPatchVDoc . to (monadError AppVDocError . canonicalizeVDocVersion)
      childphandle <- docRepo $ DocRepo.createPatch rhandle basephandle version
      db $ do
        childPatch <- DB.createPatch rid childphandle
        pure childPatch
