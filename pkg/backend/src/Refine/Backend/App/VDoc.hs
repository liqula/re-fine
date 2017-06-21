{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.VDoc where

import Refine.Backend.Prelude

import           Control.Arrow ((&&&))
import           Control.Lens ((^.), (^?), view, has)
import           Control.Monad ((<=<), mapM)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)

import           Refine.Backend.App.Core
import           Refine.Backend.App.User (currentUser)
import qualified Refine.Backend.Database.Class as DB
import           Refine.Common.Allow
import           Refine.Common.Types
import qualified Refine.Common.OT as OT
import           Refine.Common.VDoc.Draft (rawContentFromVDocVersion, deleteMarksFromRawContent)


listVDocs :: App [VDoc]
listVDocs = do
  appLog "listVDocs"
  db $ mapM DB.getVDoc =<< DB.listVDocs

-- | Create 'VDoc' and return the corresponding 'CompositeVDoc'.
createVDocGetComposite :: Create VDoc -> App CompositeVDoc
createVDocGetComposite = (getCompositeVDocOnHead . view vdocID) <=< createVDoc

-- | Creates a 'VDoc'.  See also: 'createVDocGetComposite'.
createVDoc :: Create VDoc -> App VDoc
createVDoc pv = do
  appLog "createVDoc"
  let vd = pv ^. createVDocInitVersion
  db $ DB.createVDoc pv vd

getVDoc :: ID VDoc -> App VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ DB.getVDoc i

getVDocVersion :: ID Edit -> App VDocVersion
getVDocVersion eid = do
  appLog "getVDocVersion"
  db $ DB.getVersion eid

getCompositeVDocOnHead :: ID VDoc -> App CompositeVDoc
getCompositeVDocOnHead vid = do
  vdoc <- db $ DB.getVDoc vid
  getCompositeVDoc' vdoc (vdoc ^. vdocHeadEdit)

getCompositeVDoc :: ID VDoc -> ID Edit -> App CompositeVDoc
getCompositeVDoc vdocid editid = do
  vdoc <- db $ DB.getVDoc vdocid
  getCompositeVDoc' vdoc editid

getCompositeVDoc' :: VDoc -> ID Edit -> App CompositeVDoc
getCompositeVDoc' vdoc editid = do
  appLog "getCompositeVDoc"
  edit <- db $ DB.getEdit editid
  comments <- db $ DB.editComments editid
  let commentNotes       = catMaybes $ (^? _CommentNote)       <$> filter (has _CommentNote)       comments
      commentDiscussions = catMaybes $ (^? _CommentDiscussion) <$> filter (has _CommentDiscussion) comments
  edits <- db $ mapM DB.getEdit =<< DB.getEditChildren editid
  version <- db $ DB.getVersion editid
  pure $
    CompositeVDoc
      vdoc edit version
      (toMap editID edits)
      (toMap noteID commentNotes)
      (toMap (compositeDiscussion . discussionID) commentDiscussions)
  where
    toMap selector = Map.fromList . fmap (view selector &&& id)

addEdit
  :: (MonadApp db uh, Allow (DB.ProcessPayload Edit) Edit)
  => ID Edit -> Create Edit -> AppM db uh Edit
addEdit baseeid edit = do
  appLog "addEdit"
  -- assertPerms baseeid [Create]  -- FIXME: http://zb2/re-fine/re-fine/issues/286
    -- (note that the user must have create permission on the *base
    -- edit*, not the edit about to get created.)
  db $ do
    rid <- DB.vdocOfEdit baseeid
    olddoc <- rawContentFromVDocVersion <$> DB.getVersion baseeid
    dff <- either error pure $
            -- error reporting is not great:
            -- - this is an internal error, and it may be possible to rule it out on the type level.
            -- - this should be an 'AppError', but it happens in the DB monad.
            -- - since we 'error' out sloppily, the backend console says 'SQLite3 returned ErrorError
            --   while attempting to perform step.' and the frontend says 'Error in $: Failed
            --   reading: not a valid json value'.
        OT.diff (deleteMarksFromRawContent olddoc)
                (deleteMarksFromRawContent . rawContentFromVDocVersion $ edit ^. createEditVDoc)
    childEdit <- DB.createEdit rid (EditSource [(dff, baseeid)]) edit
    pure childEdit


-- | Throw an error if chunk range does not fit 'VDocVersion' identified by edit.
-- FIXME: for RawContent this still needs to be implemented.
validateCreateChunkRange :: ID Edit -> Range Position -> App ()
validateCreateChunkRange _ _ = pure ()  -- throwError AppVDocVersionError

withCurrentUser :: (MonadApp db uh) => (ID User -> AppM db uh ()) -> AppM db uh ()
withCurrentUser f = do
  mu <- currentUser
  case mu of
    Just u -> f u
    Nothing -> throwError AppUnauthorized

putSimpleVoteOnEdit :: (MonadApp db uh) => ID Edit -> Vote -> AppM db uh ()
putSimpleVoteOnEdit eid v = withCurrentUser $ \user -> db . DB.updateVotes eid $ Map.insert user v

deleteSimpleVoteOnEdit :: (MonadApp db uh) => ID Edit -> AppM db uh ()
deleteSimpleVoteOnEdit eid = withCurrentUser $ \user -> db . DB.updateVotes eid $ Map.delete user

getSimpleVotesOnEdit :: (MonadApp db uh) => ID Edit -> AppM db uh VoteCount
getSimpleVotesOnEdit eid = db $ DB.getVoteCount eid
