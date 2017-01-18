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

import Control.Exception
import Control.Lens (makeLenses, makePrisms)
import Control.Monad.Except
import Control.Monad.Reader
import Database.Persist.Sql


data DBKind
  = DBInMemory
  | DBOnDisk FilePath

data DBConfig = DBConfig
  { _dbConfigPoolSize :: Int
  , _dbConfigDBKind   :: DBKind
  }

type SQLM = ReaderT SqlBackend IO

newtype DB a = DB { unDB :: ExceptT DBError SQLM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DBError
    )

data DBError
  = DBUnknownError String
  | DBNotFound String
  | DBNotUnique String
  | DBException SomeException
  deriving (Show)

notFound :: String -> DB a
notFound = DB . throwError . DBNotFound

notUnique :: String -> DB a
notUnique = DB . throwError . DBNotUnique

liftDB :: SQLM a -> DB a
liftDB = DB . lift

-- * lenses

makeLenses ''DBConfig

makePrisms ''DBKind
makePrisms ''DBConfig
