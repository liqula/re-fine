{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Comment where

import Refine.Backend.Prelude

import qualified Data.Set as Set
import qualified Data.Map as Map

import           Refine.Backend.App.User
import           Refine.Backend.App.Access
import           Refine.Backend.App.Core
-- import           Refine.Backend.App.VDoc (validateCreateChunkRange)
import           Refine.Backend.Database.Class as DB
import qualified Refine.Common.Access.Policy as AP
import           Refine.Common.Types
import           Refine.Common.VDoc.Draft (minimumRange)


addNote :: ID Edit -> CreateNote (Maybe (Range Position)) -> App Note
addNote eid (CreateNote txt mrange) = do
  appLog "addNote"
  assertCreds . AP.createComment =<< db (DB.getVDoc =<< DB.vdocOfEdit eid)
  note <- db $ do
    range <- case mrange of
      Just r  -> {- FIXME: validateCreateChunkRange eid r >> -} pure r
      Nothing -> minimumRange . view editVDocVersion <$> DB.getEdit eid
    DB.createNote eid (CreateNote txt range)
  invalidateCaches $ Set.fromList [CacheKeyEdit eid]
  pure note

getNote :: ID Note -> App Note
getNote nid = do
  appLog "getNote"
  note <- db $ DB.getNote nid
  assertCreds . AP.viewVDoc =<< db (DB.getVDoc $ note ^. noteVDoc)
  pure note

toggleSimpleVoteOnNote :: ID Note -> Vote -> App ()
toggleSimpleVoteOnNote i v = do
  let f (Just v') | v' == v = Nothing
      f _ = Just v
  withCurrentUser $ \user -> db . DB.updateNoteVotes i $ Map.alter f user
  invalidateCaches $ Set.fromList [CacheKeyNote i]

addDiscussion :: ID Edit -> CreateDiscussion (Maybe (Range Position)) -> App Discussion
addDiscussion eid (CreateDiscussion txt mrange) = do
  appLog "addDiscussion"
  assertCreds . AP.createComment =<< db (DB.getVDoc =<< DB.vdocOfEdit eid)
  disc <- db $ do
    range <- case mrange of
      Just r  -> {- FIXME: validateCreateChunkRange eid r >> -} pure r
      Nothing -> minimumRange . view editVDocVersion <$> DB.getEdit eid
    dscn <- DB.createDiscussion eid (CreateDiscussion txt range)
    DB.getDiscussion (dscn ^. discussionID)
  invalidateCaches $ Set.fromList [CacheKeyEdit eid]
  pure disc

getDiscussion :: ID Discussion -> App Discussion
getDiscussion did = do
  appLog "getDiscussion"
  disc <- db $ DB.getDiscussion did
  assertCreds . AP.viewVDoc =<< db (DB.getVDoc $ disc ^. discussionVDoc)
  pure disc

addStatement :: ID Statement -> CreateStatement -> App Discussion
addStatement sid statement = do
  appLog "addStatement"
  disc <- db $ do
    _ <- DB.createStatement sid statement
    DB.getDiscussion =<< DB.discussionOfStatement sid
  assertCreds . AP.createComment =<< db (DB.getVDoc $ disc ^. discussionVDoc)
  invalidateCaches $ Set.fromList [CacheKeyDiscussion $ disc ^. discussionID]
  pure disc

updateStatement :: ID Statement -> CreateStatement -> App Discussion
updateStatement sid statement = do
  appLog "updateStatement"
  disc <- db $ do
    _ <- DB.updateStatement sid statement
    DB.getDiscussion =<< DB.discussionOfStatement sid
  assertCreds . AP.updateStatement =<< db (DB.getStatement sid)
  invalidateCaches $ Set.fromList [CacheKeyDiscussion $ disc ^. discussionID]
  pure disc
