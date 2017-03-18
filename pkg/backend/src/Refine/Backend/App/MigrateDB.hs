module Refine.Backend.App.MigrateDB where

import Data.Monoid ((<>))

import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.MigrateDB as DB
import Refine.Backend.User
import Refine.Backend.User.MigrateDB as User


-- | (With dependent types, we could take a 'Config' as argument here and then return an @AppM DB
-- uh@.  But as it is, we have to have two functions, this and 'migrateDBDevMode'.
migrateDB :: AppM DB UH ()
migrateDB = do
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB True <*> User.migrateDB True
  appLog $ show mig
  appLog "Start database migration ... DONE"

migrateDBDevMode :: AppM DB FreeUH ()
migrateDBDevMode = do
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB False <*> User.migrateDB False
  appLog $ show mig
  appLog "Start database migration ... DONE"
