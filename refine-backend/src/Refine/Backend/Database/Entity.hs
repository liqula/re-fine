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

module Refine.Backend.Database.Entity where

import Control.Lens ((^.), to)
import Control.Monad (void)
import Data.Functor.Infix ((<$$>))
import Data.String.Conversions (ST)
import Data.Typeable
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Lentil.Core (entityLens)
import Lentil.Types as L

import           Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import           Refine.Backend.Database.Types
import qualified Refine.Backend.DocRepo.Core as DocRepo
import           Refine.Common.Types

-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep VDoc     = S.VDoc
type instance S.EntityRep Patch    = S.Patch
type instance S.EntityRep VDocRepo = S.Repo
type instance S.EntityRep Note     = S.Note
type instance S.EntityRep Comment  = S.Comment

{-
Reading the domain structured datatypes is not a problem,
as there is no big difference, in accessing the parts, and
combine them via an applicative functor, producing an lazy value.

The problem arises when we add some information to the computed
value in an another pure computation. The changes in the value
will be lost, as there is no triger mechanism.

If we want to address this problem, we need to use lenses over
some ids, to navigate deeper in the data structures.

If the lens only a getter, the result functor won't run the update
Whenever we operate on some data we need to load the current
version from the db.

Instead of saving the whole value in the database, a generic
diff algorithm could be used.

https://hackage.haskell.org/package/gdiff
-}

-- * Helpers

idNotFound :: (Typeable d) => ID d -> DB a
idNotFound i = notFound $ unwords [show $ typeOf i, show i, "is not found."]

-- FIXME: Better error messages
unique :: [a] -> DB a
unique [x] = pure x
unique []  = notFound "Unique value is not found"
unique _   = notUnique "Value is not unique"

getEntity :: (ToBackendKey SqlBackend (S.EntityRep e), Typeable e)
          => ID e -> DB (S.EntityRep e)
getEntity eid = do
  e <- liftDB . get $ S.idToKey eid
  maybe (idNotFound eid) pure e

-- | Access the key like field in an entity and convert the key value
-- to the Domain ID.
--
-- Example:
--   * foreignKeyField S.PCComment (Entity pcid (S.PC pid cid)) == cid
--   * foreignKeyField S.PCPatch   (Entity pcid (S.PC pid cid)) == pid
foreignKeyField
  :: ToBackendKey SqlBackend (S.EntityRep a)
  => (b -> Key (S.EntityRep a)) -> Database.Persist.Entity b -> ID a
foreignKeyField column = S.keyToId . column . entityVal

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc _i t a r) = pure (S.VDoc t a (S.idToKey r))

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc vid vdoc = do
  record <- vDocToRecord vdoc
  liftDB $ replace (S.idToKey vid) record

toChunkRange :: DBChunkRange -> ID a -> ChunkRange a
toChunkRange r i = ChunkRange i (r ^. dbChunkRangeBegin) (r ^. dbChunkRangeEnd)

mkDBChunkRange :: CreateChunkRange -> DBChunkRange
mkDBChunkRange cr = DBChunkRange (cr ^. createChunkRangeBegin) (cr ^. createChunkRangeEnd)

vdocDBLens :: EntityLens' DB (ID VDoc) VDoc
vdocDBLens = entityLens vdocEntity

vdocEntity :: L.Entity DB ID VDoc VDoc
vdocEntity = L.Entity getVDoc updateVDoc

-- * VDoc

toVDoc :: ID VDoc -> Title -> Abstract -> Key S.Repo -> VDoc
toVDoc vid title abstract repoid = VDoc vid title abstract (S.keyToId repoid)

listVDocs :: DB [ID VDoc]
listVDocs = liftDB $ S.keyToId <$$> selectKeysList [] []

createVDoc :: Create VDoc -> VDocRepo -> DB VDoc
createVDoc pv vr = liftDB $ do
  let svdoc = S.VDoc
        (pv ^. createVDocTitle)
        (pv ^. createVDocAbstract)
        (vr ^. vdocRepoID . to S.idToKey)
  key <- insert svdoc
  void . insert $ S.VR key (vr ^. vdocRepoID . to S.idToKey)
  pure $ S.vDocElim (toVDoc (S.keyToId key)) svdoc

getVDoc :: ID VDoc -> DB VDoc
getVDoc vid = S.vDocElim (toVDoc vid) <$> getEntity vid

vdocRepo :: ID VDoc -> DB (ID VDocRepo)
vdocRepo vid = do
  vs <- liftDB $ foreignKeyField S.vRRepository <$$> selectList [S.VRVdoc ==. S.idToKey vid] []
  unique vs

-- * Repo

createRepo :: DocRepo.RepoHandle -> DocRepo.PatchHandle -> DB VDocRepo
createRepo repoh patchh = liftDB $ do
    let desc = "" -- TODO
    pkey <- insert $ S.Patch desc patchh
    key  <- insert $ S.Repo "title" {- TODO -} repoh pkey
    void  . insert $ S.RP key pkey
    pure $ VDocRepo (S.keyToId key) (S.keyToId pkey)

getRepo :: ID VDocRepo -> DB VDocRepo
getRepo vid = S.repoElim toVDocRepo <$> getEntity vid
  where
    toVDocRepo :: ST -> DocRepo.RepoHandle -> Key S.Patch -> VDocRepo
    toVDocRepo _desc _repoHandle pid = VDocRepo vid (S.keyToId pid)

getRepoFromHandle :: DocRepo.RepoHandle -> DB VDocRepo
getRepoFromHandle hndl = do
  rs <- liftDB $ selectList [S.RepoRepoHandle ==. hndl] []
  r <- unique rs
  let rid = S.keyToId $ entityKey r
      toRepo _desc _hdnl repohead = VDocRepo rid (S.keyToId repohead)
  pure $ S.repoElim toRepo (entityVal r)

getRepoHandle :: ID VDocRepo -> DB DocRepo.RepoHandle
getRepoHandle vid = S.repoElim toRepoHandle <$> getEntity vid
  where
    toRepoHandle :: ST -> DocRepo.RepoHandle -> Key S.Patch -> DocRepo.RepoHandle
    toRepoHandle _desc repoHandle _pid = repoHandle

getPatchIDs :: ID VDocRepo -> DB [ID Patch]
getPatchIDs vid = liftDB $
  foreignKeyField S.rPPatch <$$> selectList [S.RPRepository ==. S.idToKey vid] []

-- * Patch

createPatch :: ID VDocRepo -> DocRepo.PatchHandle -> DB Patch
createPatch rid patchh = liftDB $ do
  let desc = "" -- TODO
  key <- insert $ S.Patch desc patchh
  void . insert $ S.RP (S.idToKey rid) key
  let pid = S.keyToId key
      cr = ChunkRange pid Nothing Nothing  -- TODO
  pure $ Patch pid desc cr

getPatch :: ID Patch -> DB Patch
getPatch pid = S.patchElim toPatch <$> getEntity pid
  where
    cr :: ChunkRange Patch
    cr = ChunkRange pid Nothing Nothing  -- TODO

    toPatch :: ST -> DocRepo.PatchHandle -> Patch
    toPatch desc _handle = Patch pid desc cr

getPatchFromHandle :: DocRepo.PatchHandle -> DB Patch
getPatchFromHandle hndl = do
  ps <- liftDB $ selectList [S.PatchPatchHandle ==. hndl] []
  p <- unique ps
  let pid = S.keyToId $ entityKey p
      cr = ChunkRange pid Nothing Nothing  -- TODO
      toPatch desc _hdnl = Patch (S.keyToId $ entityKey p) desc cr
  pure $ S.patchElim toPatch (entityVal p)

getPatchHandle :: ID Patch -> DB DocRepo.PatchHandle
getPatchHandle pid = S.patchElim toPatchHandle <$> getEntity pid
  where
    toPatchHandle :: ST -> DocRepo.PatchHandle -> DocRepo.PatchHandle
    toPatchHandle _desc handle = handle

patchComments :: ID Patch -> DB [ID Comment]
patchComments pid = liftDB $
  foreignKeyField S.pCComment <$$> selectList [S.PCPatch ==. S.idToKey pid] []

patchNotes :: ID Patch -> DB [ID Note]
patchNotes pid = liftDB $
  foreignKeyField S.pNNote <$$> selectList [S.PNPatch ==. S.idToKey pid] []

-- * Repo and patch

patchVDocRepo :: ID Patch -> DB (ID VDocRepo)
patchVDocRepo pid = do
  rs <- liftDB $ foreignKeyField S.rPRepository <$$> selectList [S.RPPatch ==. S.idToKey pid] []
  unique rs

registerPatch :: ID VDocRepo -> ID Patch -> DB ()
registerPatch rid pid = void . liftDB . insert $ S.RP (S.idToKey rid) (S.idToKey pid)

-- * Comment

toComment :: ID Comment -> ST -> Bool -> DBChunkRange -> Maybe (Key S.Comment) -> Comment
toComment cid desc public range _parent = Comment cid desc public (toChunkRange range cid)

createComment :: ID Patch -> Create Comment -> DB Comment
createComment pid comment = liftDB $ do
  let scomment = S.Comment
        (comment ^. createCommentText)
        (comment ^. createCommentPublic)
        (comment ^. createCommentRange . to mkDBChunkRange)
        Nothing
  key <- insert scomment
  void . insert $ S.PC (S.idToKey pid) key
  pure $ S.commentElim (toComment (S.keyToId key)) scomment

getComment :: ID Comment -> DB Comment
getComment cid = S.commentElim (toComment cid) <$> getEntity cid

-- * Note

toNote :: ID Note -> ST -> NoteKind -> DBChunkRange -> Note
toNote nid desc kind range = Note nid desc kind (toChunkRange range nid)

createNote :: ID Patch -> Create Note -> DB Note
createNote pid note = liftDB $ do
  let snote = S.Note
        (note ^. createNoteText)
        (note ^. createNoteKind)
        (note ^. createNoteRange . to mkDBChunkRange)
  key <- insert snote
  void . insert $ S.PN (S.idToKey pid) key
  pure $ S.noteElim (toNote (S.keyToId key)) snote

getNote :: ID Note -> DB Note
getNote nid = S.noteElim (toNote nid) <$> getEntity nid
