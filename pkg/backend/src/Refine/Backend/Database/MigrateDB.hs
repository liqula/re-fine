module Refine.Backend.Database.MigrateDB where

import Data.Monoid ((<>))
import Data.String.Conversions (ST)
import Database.Persist.Sql
import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema


migrateDB :: DB [ST]
migrateDB = liftDB $ do
  mig'  <- showMigration migrateRefine
  mig'' <- runMigrationSilent migrateRefine
  pure $ mig' <> mig''
