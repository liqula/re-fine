module Refine.Backend.App.MigrateDB where

import Data.Monoid ((<>))

import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.MigrateDB as DB
import Refine.Backend.User.Core as User


migrateDB :: App DB ()
migrateDB = do
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB <*> User.migrateDB
  appLog $ show mig
  appLog "Start database migration ... DONE"
