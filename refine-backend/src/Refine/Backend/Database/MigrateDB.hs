module Refine.Backend.Database.MigrateDB where

import Data.String.Conversions (ST)
import Database.Persist.Sql
import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema



migrateDB :: DB [ST]
migrateDB = do
  liftDB $ do
    mig' <- showMigration migrateRefine
    runMigration migrateRefine
    pure mig'
