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

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module hides away the fact of the usage of the Users
-- library.
module Refine.Backend.User.Core
  ( UserDB
  , UserHandleContext(..), userBackend
  , UserHandleError
  , UHNat
  , migrateDB
  , toUserID
  , fromUserID
  , Login
  , LoginId
  , CreateUserError(..)
  , SessionId(..)
  , PasswordPlain(..)
  , User(..)
  , makePassword
  ) where

import Control.Lens (makeLenses)
import Control.Monad.Except
import Control.Natural
import Data.Monoid
import Data.String.Conversions (ST)
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.Users.Types (CreateUserError(..), SessionId(..), PasswordPlain(..), User(..), makePassword)
import Web.Users.Persistent as Users
import Web.Users.Persistent.Definitions (Login, migrateAll)

import Refine.Backend.Database.Core
import Refine.Common.Types.Prelude (ID(..))
import qualified Refine.Common.Types.User as Types (User)
import Refine.Prelude.TH (makeRefineType)


type UserDB = Users.Persistent

newtype UserHandleContext = UserHandleContext
  { _userBackend :: UserDB
  }

makeLenses ''UserHandleContext

data UserHandleError
  = UserHandleUnknownError
  deriving (Eq, Generic, Show)

makeRefineType ''UserHandleError

type UHNat uh = uh :~> ExceptT UserHandleError IO

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
