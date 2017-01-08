{-# LANGUAGE FlexibleContexts #-}
module Refine.Backend.Database.Entity where

import Lentil.Core (entityLens)
import Lentil.Types as L

import Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import Refine.Common.Types

import Database.Persist


-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep VDoc = S.VDoc

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

vdocDBLens :: EntityLens' DB (ID VDoc) VDoc
vdocDBLens = entityLens vdocEntity

vdocEntity :: L.Entity DB ID VDoc VDoc
vdocEntity = L.Entity loadVDoc updateVDoc

loadVDoc :: ID VDoc -> DB VDoc
loadVDoc vid =
  (liftDB . get $ S.idToKey vid) >>=
    (maybe
      (notFound $ unwords ["VDoc", show vid, " is not found"])
      (pure . S.vDocElim VDoc))

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc t d) = pure (S.VDoc t d)

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc vid vdoc = do
  record <- vDocToRecord vdoc
  liftDB $ replace (S.idToKey vid) record

