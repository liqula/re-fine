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

module Refine.Backend.App.Core where

import Control.Lens (makeLenses, view)
import Control.Monad.Except

import Control.Natural
import Control.Monad.Reader

import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Common.VDoc.HTML (VDocHTMLError)



type RunDB db   = db      :~> ExceptT DBError      IO
type RunDocRepo = DocRepo :~> ExceptT DocRepoError IO

data AppContext db = AppContext
  { _appRunDB      :: RunDB db
  , _appRunDocRepo :: RunDocRepo
  , _appLogger     :: Logger
  }

makeLenses ''AppContext

-- | Application monad handles
-- * database connection
-- * event logging
-- TODO:
-- * user authorization
-- * user authentication
-- * use one db connection in one run.
newtype App db a = App { unApp :: ReaderT (AppContext db) (ExceptT AppError IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppContext db)
    , MonadError AppError
    )

data AppError
  = AppUnknownError String
  | AppVDocError VDocHTMLError
  | AppDBError DBError
  | AppDocRepoError DocRepoError
  deriving (Show)

appIO :: IO a -> App db a
appIO = App . liftIO

db :: db a -> App db a
db m = App $ do
  (Nat runDB) <- view appRunDB
  r <- liftIO (runExceptT (runDB m))
  either (throwError . AppDBError) pure r

docRepo :: DocRepo a -> App db a
docRepo m = App $ do
  (Nat runDocRepo) <- view appRunDocRepo
  r <- liftIO (runExceptT (runDocRepo m))
  either (throwError . AppDocRepoError) pure r

appLog :: String -> App db ()
appLog msg = App $ do
  logger <- view appLogger
  liftIO $ unLogger logger msg
