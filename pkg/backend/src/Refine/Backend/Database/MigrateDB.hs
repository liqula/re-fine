module Refine.Backend.Database.MigrateDB where

import Control.Monad.Except
import Data.String.Conversions (ST)
import Database.Persist.Sql

import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema


migrateDB :: DB [ST]
migrateDB = do
  result <- liftDB $ parseMigration migrateRefine
  case result of
    Left parseErrors ->
      throwError $ DBMigrationParseErrors parseErrors

    Right cautiousMigration ->
      unless (null $ filter fst cautiousMigration) $
        throwError $ DBUnsafeMigration cautiousMigration

  liftDB $ runMigrationSilent migrateRefine
