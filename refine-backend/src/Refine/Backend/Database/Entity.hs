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
import Database.Persist
import Data.String.Conversions (ST)
import Data.Typeable
import Lentil.Core (entityLens)
import Lentil.Types as L

import Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import qualified Refine.Backend.DocRepo.Core as DocRepo
import Refine.Common.Types


-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep VDoc     = S.VDoc
type instance S.EntityRep Patch    = S.Patch
type instance S.EntityRep VDocRepo = S.Repo

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
    toVDoc :: Title -> Abstract -> Key S.Repo -> VDoc
    toVDoc title abstract repoid = VDoc vid title abstract (S.keyToId repoid)

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc _i t a r) = pure (S.VDoc t a (S.idToKey r))

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc vid vdoc = do
  record <- vDocToRecord vdoc
  liftDB $ replace (S.idToKey vid) record


loadPatch :: ID Patch -> DB Patch
loadPatch pid =
  (liftDB . get $ S.idToKey pid) >>=
    maybe
      (idNotFound pid)
      (pure . S.patchElim toPatch)
  where
    cr :: ChunkRange Patch
    cr = ChunkRange pid Nothing Nothing  -- TODO

    toPatch :: ST -> DocRepo.PatchHandle -> Patch
    toPatch desc _handle = Patch pid desc cr

createRepo :: DocRepo.RepoHandle -> ID Patch -> DB VDocRepo
createRepo repoh pid = do
  key <- liftDB $ do
    key <- insert $ S.Repo "title" {- TODO -} repoh (S.idToKey pid)
    void . insert $ S.RC key (S.idToKey pid)
    pure key
  pure $ VDocRepo (S.keyToId key) pid

createPatch :: DocRepo.PatchHandle -> DB Patch
createPatch p = do
  let desc = "" -- TODO
  key <- liftDB . insert $ S.Patch desc p
  let pid = S.keyToId key
      cr = ChunkRange pid Nothing Nothing  -- TODO
  pure $ Patch pid desc cr

createVDoc :: Proto VDoc -> VDocRepo -> DB VDoc
createVDoc pv vr = do
  key <- liftDB . insert $ S.VDoc
            (pv ^. protoVDocTitle)
            (pv ^. protoVDocAbstract)
            (vr ^. vdocRepoID . to S.idToKey)
  pure $ VDoc
    (S.keyToId key)
    (pv ^. protoVDocTitle)
    (pv ^. protoVDocAbstract)
    (vr ^. vdocRepoID)
