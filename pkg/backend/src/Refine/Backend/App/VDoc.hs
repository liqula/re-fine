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
createVDocGetComposite = (getCompositeVDoc . view vdocID) <=< createVDoc

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

getCompositeVDoc :: ID VDoc -> App CompositeVDoc  -- TODO: take an edit id here, and implement getHeadCompositeVDoc in terms of that.
getCompositeVDoc vid = do
  appLog "getCompositeVDoc"
  vdoc     <- db $ DB.getVDoc vid
  let headid = vdoc ^. vdocHeadEdit
  comments <- db $ DB.editComments headid
  let commentNotes       = catMaybes $ (^? _CommentNote)       <$> filter (has _CommentNote)       comments
      commentDiscussions = catMaybes $ (^? _CommentDiscussion) <$> filter (has _CommentDiscussion) comments

  edits <- db $ mapM DB.getEdit =<< DB.getEditChildren headid
  version <- db $ DB.getVersion headid
  pure $
    CompositeVDoc
      vdoc headid version
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
  -- assertPerms baseeid [Create]  -- TODO: http://zb2/re-fine/re-fine/issues/286
    -- (note that the user must have create permission on the *base
    -- edit*, not the edit about to get created.)
  validateCreateChunkRange baseeid (edit ^. createEditRange)
  db $ do
    rid <- DB.vdocOfEdit baseeid
    olddoc <- rawContentFromVDocVersion <$> DB.getVersion baseeid
    childEdit <- DB.createEdit rid (EditOfEdit (fromMaybe (error "can't diff documents!") $
        OT.diff (deleteMarksFromRawContent olddoc)
                (deleteMarksFromRawContent . rawContentFromVDocVersion $ edit ^. createEditVDoc)
        ) baseeid) edit
    pure childEdit


-- | Throw an error if chunk range does not fit 'VDocVersion' identified by edit.
-- FIXME: for RawContent this still needs to be implemented.
validateCreateChunkRange :: ID Edit -> ChunkRange -> App ()
validateCreateChunkRange _ _ = pure ()  -- throwError AppVDocVersionError
