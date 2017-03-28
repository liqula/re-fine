{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.App.MigrateDB where

import Control.Lens
import Control.Monad ((=<<))
import Data.Monoid ((<>))

import Refine.Backend.Config
import Refine.Backend.App.Core
import Refine.Backend.Database.Core
import Refine.Backend.Database.Class     as DB
import Refine.Backend.Database.MigrateDB as DB
import Refine.Backend.User.MigrateDB as User
import Refine.Common.Types.Group (CreateGroup(..))


-- | (With dependent types, we could take a 'Config' as argument here and then return an @AppM DB
-- uh@.  But as it is, we have to have two functions, this and 'migrateDBDevMode'.)
migrateDB :: Config -> AppM DB uh ()
migrateDB cfg = do
  let safety = if cfg ^. cfgDevMode then UnsafeMigration else SafeMigration
  appLog "Start database migration ..."
  mig <- db $ (<>) <$> DB.migrateDB safety <*> User.migrateDB safety
  appLog $ show mig
  appLog "Start database migration ... DONE"

createInitialDB :: AppM DB uh ()
createInitialDB = do
  appLog "Create initial database state ..."
  appLog . show =<< db (DB.createGroup (CreateGroup "Universal" "Group for all of the users" [] [] True))
  appLog "Create initial database state ... DONE"
