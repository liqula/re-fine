{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.App.MigrateDB where
#include "import.hs"

import Refine.Backend.Config
import Refine.Backend.App
import Refine.Backend.Database.Core
import Refine.Backend.Database.Class     as DB
import Refine.Backend.Database.MigrateDB as DB


-- | (With dependent types, we could take a 'Config' as argument here and then return an @AppM DB
-- uh@.  But as it is, we have to have two functions, this and 'migrateDBDevMode'.)
migrateDB :: Config -> AppM DB ()
migrateDB _cfg = do
  appLog "Start database migration ..."
  mig <- db $ DB.migrateDB SafeMigration
  appLog $ show mig
  appLog "Start database migration ... DONE"

-- | (This cannot be easily moved to "Refine.Backend.Config" due to import cycles.)
initializeDB :: [CliCreate] -> AppM DB ()
initializeDB xs = do
  appLog "Create initial database state: STARTING"
  forM_ xs $ \case
    CliCreateGroup cg -> do
      appLogL LogInfo . show =<< tryApp (db $ DB.createGroup cg)
    CliCreateUser (cu, lrs, grs) -> do
      lrs' <- do
        unless (null lrs) $ do
          appLogL LogWarning "*** WARNING: group roles for users are not supported, ignored."  -- FIXME
        pure []
      appLogL LogInfo . show =<< tryApp (createUserWith grs lrs' cu)

  appLog "Create initial database state: OK"
