module Refine.Backend.Database.MigrateDB where

import Control.Monad.Except
import Data.String.Conversions (ST)
import Database.Persist.Sql

import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema


-- | Run the migration.
migrateDB :: Bool -> DB [ST]
migrateDB = doMigrate migrateRefine

doMigrate :: Migration -> Bool -> DB [ST]

-- Nonsafe migration
doMigrate migration False = liftDB $ do
  mig <- getMigration migration
  runMigrationUnsafe migration
  pure mig

-- Safe migration
doMigrate migration True = do
  result <- liftDB $ parseMigration migration
  case result of
    Left parseErrors ->
      throwError $ DBMigrationParseErrors parseErrors

    Right cautiousMigration ->
      unless (null $ filter fst cautiousMigration) $
        throwError $ DBUnsafeMigration cautiousMigration

  liftDB $ runMigrationSilent migration
