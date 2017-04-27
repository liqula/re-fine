{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Refine.Backend.Database.Core where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.String.Conversions (ST)
import Database.Persist.Sql hiding (Filter)
import GHC.Generics (Generic)

import Refine.Prelude (HasCurrentTime(..))
import Refine.Common.Types.Prelude (ID(..), User)
import Refine.Prelude.TH (makeRefineType)


type SQLM = ReaderT SqlBackend IO

data DBContext = DBContext
  { _dbLoggedInUser :: Maybe (ID User)
  , _dbFilter       :: Filters
  }

-- FIXME: follow the structure as in "Refine.Backend.User.*" (here as well as in "...DocRepo").
-- this may introduce some circular dependencies, but we may be able to resolve them with creating
-- more sub-modules, like splitting up "Refine.Backend.Database.DB" into
-- "Refine.Backend.Database.DB" and "Refine.Backend.Database.DB.Type".
newtype DB a = DB { unDB :: ExceptT DBError (ReaderT DBContext SQLM) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DBError
    , MonadReader DBContext
    )

data DBError
  = DBUnknownError String  -- ^ FUTUREWORK: make this 'SomeException'?
  | DBNotFound String
  | DBNotUnique String
  | DBException String     -- ^ FUTUREWORK: make this 'SomeException'?
  | DBUserNotLoggedIn
  | DBMigrationParseErrors [ST]
  | DBUnsafeMigration [(Bool, ST)]
  deriving (Eq, Show, Generic)

instance HasCurrentTime DB where
  getCurrentTimestamp = DB $ liftIO getCurrentTimestamp

-- | Filters the queries in the database.
-- Userful to implement pagination, uniqueness etc.
--
-- FIXME: In its current state this is not very practical, as it requires the output to be ordered
-- by default (which we probably never want), and it does not let you specify a page number.  The
-- next step (if we want pagination and not, say, filtering by full-text search), could be @data
-- Filter = Paginate PageNum PageLength | Sort@.
newtype Filter = Limit Int

type Filters = [Filter]

makeRefineType ''DBError

makeLenses ''DBContext

notFound :: String -> DB a
notFound = DB . throwError . DBNotFound

notUnique :: String -> DB a
notUnique = DB . throwError . DBNotUnique

liftDB :: SQLM a -> DB a
liftDB = DB . lift . lift
