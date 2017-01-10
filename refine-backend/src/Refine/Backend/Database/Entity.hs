{-# LANGUAGE FlexibleContexts #-}
module Refine.Backend.Database.Entity where

import Control.Lens ((^.), to)
import Control.Monad (void)
import Data.Typeable
import Lentil.Core (entityLens)
import Lentil.Types as L

import Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import qualified Refine.Backend.DocRepo.Core as DocRepo
import Refine.Common.Types

import Database.Persist


-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep VDoc           = S.VDoc
type instance S.EntityRep Patch          = S.Commit
type instance S.EntityRep VDocRepository = S.Repository

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

idNotFound :: (Typeable d) => ID d -> DB a
idNotFound i = notFound $ unwords [show $ typeOf i, show i, "is not found."]

vdocDBLens :: EntityLens' DB (ID VDoc) VDoc
vdocDBLens = entityLens vdocEntity

vdocEntity :: L.Entity DB ID VDoc VDoc
vdocEntity = L.Entity loadVDoc updateVDoc

loadVDoc :: ID VDoc -> DB VDoc
loadVDoc vid =
  (liftDB . get $ S.idToKey vid) >>=
    maybe
      (idNotFound vid)
      (pure . S.vDocElim toVDoc)
  where
    toVDoc t d r = VDoc vid t d (S.keyToId r)

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc _i t d r) = pure (S.VDoc t d (S.idToKey r))

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc vid vdoc = do
  record <- vDocToRecord vdoc
  liftDB $ replace (S.idToKey vid) record


patchEntity :: L.Entity DB ID Patch Patch
patchEntity = L.Entity loadPatch updatePatch

-- TODO: Rename patch to commit
loadPatch :: ID Patch -> DB Patch
loadPatch pid =
  (liftDB . get $ S.idToKey pid) >>=
    maybe
      (idNotFound pid)
      (pure . S.commitElim (Patch pid))

patchToRecord :: Patch -> DB S.Commit
patchToRecord (Patch _id d h) = pure (S.Commit d h)

updatePatch :: ID Patch -> Patch -> DB ()
updatePatch pid patch = do
  record <- patchToRecord patch
  liftDB $ replace (S.idToKey pid) record

createRepo :: DocRepo.Repository -> ID Patch -> DB VDocRepository
createRepo (DocRepo.Repository name repoId) pid = do
  key <- liftDB $ do
    key <- insert $ S.Repository name repoId (S.idToKey pid)
    void . insert $ S.RC key (S.idToKey pid)
    pure key
  pure $ VDocRepository (S.keyToId key) name repoId pid

createPatch :: DocRepo.Patch -> DB Patch
createPatch c = do
  let desc = "" -- TODO
  key <- liftDB . insert $ S.Commit desc (c ^. DocRepo.patchID)
  pure $ Patch (S.keyToId key) desc (c ^. DocRepo.patchID)

createVDoc :: Proto VDoc -> VDocRepository -> DB VDoc
createVDoc pv vr = do
  key <- liftDB . insert $ S.VDoc
            (pv ^. protoVDocTitle)
            (pv ^. protoVDocDescription)
            (vr ^. vdocRepositoryId . to S.idToKey)
  pure $ VDoc
    (S.keyToId key)
    (pv ^. protoVDocTitle)
    (pv ^. protoVDocDescription)
    (vr ^. vdocRepositoryId)
