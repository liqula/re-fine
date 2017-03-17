module Refine.Backend.Database.MigrateDB where

import Control.Monad.Except
import Data.String.Conversions (ST)
import Database.Persist.Sql

import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema


-- | Run the migration.
migrateDB :: Bool -> DB [ST]
migrateDB = doMigrate migrateRefine


-- NOTE: Migration can be done better, Spivak research supports
-- this theory:
-- http://math.mit.edu/~dspivak/informatics/FunctorialDataMigration.pdf
-- Spivak has contributed to the Opaleye project, maybe that could be
-- used for automatic migration... (?)
-- http://hackage.haskell.org/package/opaleye
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
      unless (not (any fst cautiousMigration)) .
        throwError $ DBUnsafeMigration cautiousMigration

  liftDB $ runMigrationSilent migration
