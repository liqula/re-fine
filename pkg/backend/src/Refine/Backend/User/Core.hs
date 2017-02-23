{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module hides away the fact of the usage of the Users
-- library.
module Refine.Backend.User.Core
  ( UserHandle
  , migrateDB
  , toUserID
  , fromUserID
  , module Web.Users.Types
  , Login
  , LoginId
  ) where

import Data.Monoid
import Data.String.Conversions (ST)
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.Users.Types
import Web.Users.Persistent as Users
import Web.Users.Persistent.Definitions (Login, migrateAll)

import Refine.Backend.Database.Core
import Refine.Common.Types.Prelude (ID(..))
import Refine.Common.Types.User as Types (User)
import Refine.Prelude.TH (makeRefineType)


type UserHandle = Users.Persistent

deriving instance Generic CreateUserError

makeRefineType ''CreateUserError

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

-- Converts an internal UserID representation to the common UserID.
toUserID :: Users.LoginId -> ID Types.User
toUserID = ID . fromSqlKey

fromUserID :: ID Types.User -> Users.LoginId
fromUserID (ID i) = toSqlKey i
