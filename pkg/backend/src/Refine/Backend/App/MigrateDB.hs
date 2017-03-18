module Refine.Backend.App.MigrateDB where

import Control.Lens
import Data.Monoid ((<>))

import Refine.Backend.Config
import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.MigrateDB as DB
import Refine.Backend.User.MigrateDB as User


-- | (With dependent types, we could take a 'Config' as argument here and then return an @AppM DB
-- uh@.  But as it is, we have to have two functions, this and 'migrateDBDevMode'.)
migrateDB :: Config -> AppM DB uh ()
migrateDB cfg = do
  let safety = if cfg ^. cfgDevMode then UnsafeMigration else SafeMigration
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB safety <*> User.migrateDB safety
  appLog $ show mig
  appLog "Start database migration ... DONE"
