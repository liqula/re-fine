{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}

-- | This module hides away the fact of the usage of the Users
-- library.
module Refine.Backend.User.Core
  ( UserHandle
  , migrateDB
  , module Web.Users.Types
  ) where

import Data.Monoid
import Data.String.Conversions (ST)
import Database.Persist.Sql
import Web.Users.Types
import Web.Users.Persistent as Users
import Web.Users.Persistent.Definitions (migrateAll)

import Refine.Backend.Database.Core


type UserHandle = Users.Persistent

-- The same db is used to store user data as the
-- application data. This could change in the future,
-- but at the moment the migration should happen at the same
-- time and with the same abstraction as the application
-- m igration.
migrateDB :: DB [ST]
migrateDB = liftDB $ do
  mig'  <- showMigration migrateAll
  mig'' <- runMigrationSilent migrateAll
  pure $ mig' <> mig''

