module Refine.Backend.App.MigrateDB where

import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.MigrateDB as DB



migrateDB :: App DB ()
migrateDB = do
  appLog "Start database migration ..."
  mig <- db DB.migrateDB
  appLog $ show mig
  appLog "Start database migration ... DONE"
