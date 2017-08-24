{-# LANGUAGE CPP #-}
#include "language.hs"
module Refine.Backend.Database.MigrateDB where

import Refine.Backend.Prelude

import Database.Persist.Sql
import qualified Web.Users.Persistent.Definitions as Users

import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema


data MigrationSafety = SafeMigration | UnsafeMigration

-- | Run the migration.
migrateDB :: MigrationSafety -> DB [ST]
migrateDB = doMigrate (migrateRefine >> Users.migrateAll)


-- | NOTE: Migration can be done better, Spivak research supports
-- this theory:
-- http://math.mit.edu/~dspivak/informatics/FunctorialDataMigration.pdf
-- Spivak has contributed to the Opaleye project, maybe that could be
-- used for automatic migration?
-- http://hackage.haskell.org/package/opaleye
doMigrate :: Migration -> MigrationSafety -> DB [ST]

doMigrate migration UnsafeMigration = liftDB $ do
  liftIO $ putStrLn "Migration to execute, when fails manual intervention is needed."
  printMigration migration
  mig <- getMigration migration
  runMigrationUnsafe migration
  pure mig

doMigrate migration SafeMigration = do
  result <- liftDB $ parseMigration migration
  case result of
    Left parseErrors ->
      throwError $ DBMigrationParseErrors parseErrors

    Right cautiousMigration ->
      unless (not (any fst cautiousMigration)) .
        throwError $ DBUnsafeMigration cautiousMigration

  liftDB $ runMigrationSilent migration
