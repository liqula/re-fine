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
import Control.Monad.Reader
import Control.Monad.State
import Control.Natural
import Data.String.Conversions (ST)
import Web.Users.Types (SessionId)

import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.User
import Refine.Common.VDoc.HTML (VDocHTMLError)



type RunDB db   = db      :~> ExceptT DBError      IO
type RunDocRepo = DocRepo :~> ExceptT DocRepoError IO

data AppContext db = AppContext
  { _appRunDB      :: RunDB db
  , _appRunDocRepo :: RunDocRepo
  , _appLogger     :: Logger
  , _appUserHandle :: UserHandle
  }

newtype UserSession = UserSession { _unUserSession :: SessionId }
  deriving (Eq, Show)

-- | The state of the application depends on the user state.
data AppUserState
  = ActiveUser UserSession
  | NonActiveUser
  deriving (Eq, Show)

makeLenses ''AppContext
makeLenses ''AppUserState
makeLenses ''UserSession

-- | Application monad handles
-- * database connection
-- * event logging
-- TODO:
-- * user authentication (login)
-- * user authorization (groups)
-- * use one db connection in one run.
newtype App db a = App { unApp :: StateT AppUserState (ReaderT (AppContext db) (ExceptT AppError IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppContext db)
    , MonadError AppError
    , MonadState AppUserState
    )

data AppError
  = AppUnknownError ST
  | AppVDocError VDocHTMLError
  | AppDBError DBError
  | AppDocRepoError DocRepoError
  | AppUserNotFound ST
  | AppUserHasNoSession
  | AppUserCreationError ST
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
