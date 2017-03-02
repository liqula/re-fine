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

import Control.Monad.Except
import Control.Monad.Reader
import Database.Persist.Sql
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude (ID(..))
import Refine.Common.Types.User (User)
import Refine.Prelude.TH (makeRefineType)


type SQLM = ReaderT SqlBackend IO

newtype DBContext = DBContext
  { _dbLoggedInUser :: Maybe (ID User)
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
  = DBUnknownError String
  | DBNotFound String
  | DBNotUnique String
  | DBException String
  | DBUserNotLoggedIn
  deriving (Eq, Show, Generic)

makeRefineType ''DBError

notFound :: String -> DB a
notFound = DB . throwError . DBNotFound

notUnique :: String -> DB a
notUnique = DB . throwError . DBNotUnique

liftDB :: SQLM a -> DB a
liftDB = DB . lift . lift
