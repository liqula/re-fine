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

import           Control.Arrow ((&&&))
import           Control.Lens ((&), (^.), (^?), to, view, has)
import           Control.Monad.Except (throwError)
import           Control.Monad ((<=<), join, mapM)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)

import           Refine.Backend.App.Core
import           Refine.Backend.Database (DB)
import qualified Refine.Backend.Database.Class as DB
import qualified Refine.Backend.DocRepo as DocRepo
import           Refine.Common.Types.Chunk
import           Refine.Common.Types.Comment
import           Refine.Common.Types.Contribution
import           Refine.Common.Types.Prelude
import           Refine.Common.Types.VDoc
import           Refine.Common.VDoc.HTML


listVDocs :: App DB [VDoc]
listVDocs = do
  appLog "listVDocs"
  db $ mapM DB.getVDoc =<< DB.listVDocs

createVDocGetComposite :: Create VDoc -> App DB CompositeVDoc
createVDocGetComposite = (getCompositeVDoc . view vdocID) <=< createVDoc

createVDoc :: Create VDoc -> App DB VDoc
createVDoc pv = do
  appLog "createVDoc"
  let vd = pv ^. createVDocInitVersion . to canonicalizeVDocVersion
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

getVDocVersion :: ID Edit -> App DB (VDocVersion 'HTMLCanonical)
getVDocVersion eid = do
  appLog "getVDocVersion"
  join . db $ do
    rid      <- DB.vdocRepoOfEdit eid
    rhandle  <- DB.getRepoHandle rid
    ehandle  <- DB.getEditHandle eid
    pure . docRepo $ DocRepo.getVersion rhandle ehandle

getCompositeVDoc :: ID VDoc -> App DB CompositeVDoc  -- TODO: take an edit id here, and implement getHeadCompositeVDoc in terms of that.
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

    pure $ do
      edits <- db $ mapM DB.getEdit =<< DB.getEditChildren headid
      let insertAllMarks :: VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
          insertAllMarks vers = vers
                              & insertMarks     (ContribNote <$> commentNotes)
                              & insertMoreMarks (ContribDiscussion . view compositeDiscussion <$> commentDiscussions)
                              & insertMoreMarks (ContribEdit <$> edits)

      version <- insertAllMarks <$> docRepo (DocRepo.getVersion rhandle hhandle)
      pure $
        CompositeVDoc
          vdoc repo version
          (toMap editID edits)
          (toMap noteID commentNotes)
          (toMap (compositeDiscussion . discussionID) commentDiscussions)
  where
    toMap selector = Map.fromList . fmap (view selector &&& id)

addEdit :: ID Edit -> Create Edit -> App DB Edit
addEdit basepid edit = do
  appLog "addEdit"
  validateCreateChunkRange basepid (edit ^. createEditRange)
  join . db $ do
    rid                    <- DB.editVDocRepo basepid
    (rhandle, basephandle) <- DB.handlesForEdit basepid
    pure $ do
      let version   = edit ^. createEditVDoc . to canonicalizeVDocVersion
      childphandle <- docRepo $ DocRepo.createEdit rhandle basephandle version
      db $ do
        childEdit <- DB.createEdit rid childphandle
        DB.setEditChild basepid (childEdit ^. editID)
        pure childEdit


-- | Throw an error if chunk range does not fit 'VDocVersion' identified by edit.
validateCreateChunkRange :: ID Edit -> CreateChunkRange -> App DB ()
validateCreateChunkRange pid cr = do
  vers <- getVDocVersion pid
  case createChunkRangeErrors cr vers of
    errs@(_:_) -> throwError $ AppVDocError errs
    [] -> pure ()
