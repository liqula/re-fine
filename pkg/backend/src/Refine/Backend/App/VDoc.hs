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

import Control.Lens ((^.), (^?), to, view, has)
import Control.Monad ((<=<), join, mapM)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)

import           Refine.Backend.App.Core
import           Refine.Backend.Database (DB)
import qualified Refine.Backend.Database.Class as DB
import qualified Refine.Backend.DocRepo as DocRepo
import           Refine.Common.Types.Chunk
import           Refine.Common.Types.Comment
import           Refine.Common.Types.Prelude
import           Refine.Common.Types.VDoc
import           Refine.Common.VDoc.HTML
import           Refine.Prelude (Void, clearTypeParameter, monadError)


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
    dp <- DocRepo.createInitialEdit dr vd
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
    let headid = repo ^. vdocHeadEdit
    hhandle  <- DB.getEditHandle headid
    comments <- DB.editComments headid
    let commentNotes       = catMaybes $ (^? _CommentNote)       <$> filter (has _CommentNote)       comments
        commentDiscussions = catMaybes $ (^? _CommentDiscussion) <$> filter (has _CommentDiscussion) comments
        chunkRangesCN = commentChunkRange <$> comments

    pure $ do
      hedits <- docRepo $ DocRepo.getChildEdits rhandle hhandle
      edits  <- db $ mapM DB.getEditFromHandle hedits
      let chunkRanges = chunkRangesCN <> (view (editRange . to clearTypeParameter) <$> edits)
      version <- monadError AppVDocError
                 =<< insertMarks chunkRanges <$> docRepo (DocRepo.getVersion rhandle hhandle)
      pure $ CompositeVDoc vdoc repo version edits commentNotes commentDiscussions

  where
    commentChunkRange :: Comment -> ChunkRange Void
    commentChunkRange = \case
      CommentNote n       -> n ^. noteChunkRange . to clearTypeParameter
      CommentQuestion q   -> q ^. compositeQuestion . questionChunkRange . to clearTypeParameter
      CommentDiscussion d -> d ^. compositeDiscussion . discussionChunkRange . to clearTypeParameter

addEdit :: ID Edit -> Create Edit -> App DB Edit
addEdit basepid edit = do
  appLog "addEdit"
  join . db $ do
    rid                    <- DB.editVDocRepo basepid
    (rhandle, basephandle) <- DB.handlesForEdit basepid
    pure $ do
      version      <- edit ^. createEditVDoc . to (monadError AppVDocError . canonicalizeVDocVersion)
      childphandle <- docRepo $ DocRepo.createEdit rhandle basephandle version
      db $ do
        childEdit <- DB.createEdit rid childphandle
        pure childEdit
