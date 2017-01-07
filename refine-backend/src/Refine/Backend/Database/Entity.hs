{-# LANGUAGE FlexibleContexts #-}
module Refine.Backend.Database.Entity where

import Lentil.Types

import Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import Refine.Common.Types

import qualified Database.Persist     as P
import qualified Database.Persist.Sql as P


type family EntityRep c = b | b -> c

type instance EntityRep VDoc = S.VDoc


idToKey :: (P.ToBackendKey P.SqlBackend (EntityRep a))
        => ID a -> P.Key (EntityRep a)
idToKey (ID vid) = P.toSqlKey vid


vdocEntity :: Entity DB ID VDoc VDoc
vdocEntity = Entity loadVDoc updateVDoc

loadVDoc :: ID VDoc -> DB VDoc
loadVDoc vid =
  (liftDB . P.get $ idToKey vid) >>=
    (maybe
      (notFound $ unwords ["VDoc", show vid, " is not found"])
      (pure . S.vDocElim VDoc))

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc _vid _vdoc = pure ()
