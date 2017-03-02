module Refine.Backend.App.MigrateDB where

import Data.Monoid ((<>))

import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.MigrateDB as DB
import Refine.Backend.User.Core as User
import Refine.Backend.User.Free (FreeUH)


migrateDB :: AppM DB UH ()
migrateDB = do
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB <*> User.migrateDB
  appLog $ show mig
  appLog "Start database migration ... DONE"

migrateDBDevMode :: AppM DB FreeUH ()
migrateDBDevMode = do
  appLog "Start database migration ..."
  mig <- db DB.migrateDB
  appLog $ show mig
  appLog "Start database migration ... DONE"
