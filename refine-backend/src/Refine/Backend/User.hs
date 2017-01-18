{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Refine.Backend.User where

import Data.Monoid
import Data.String.Conversions (ST)
import Database.Persist.Sql
import Web.Users.Persistent as Users
import Web.Users.Persistent.Definitions (migrateAll)

import Refine.Backend.Database.Core


type UserHandle = Users.Persistent

migrateUserDB :: DB [ST]
migrateUserDB = liftDB $ do
  mig'  <- showMigration migrateAll
  mig'' <- runMigrationSilent migrateAll
  pure $ mig' <> mig''

