module Refine.Backend.App.MigrateDB where

import Data.Monoid ((<>))

import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.MigrateDB as DB
import Refine.Backend.User as UserDB


migrateDB :: App DB ()
migrateDB = do
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB <*> UserDB.migrateUserDB
  appLog $ show mig
  appLog "Start database migration ... DONE"
