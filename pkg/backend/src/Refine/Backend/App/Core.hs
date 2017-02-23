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

module Refine.Backend.App.Core (
    RunDB
  , RunDocRepo
  , AppContext(..)
  , appRunDB
  , appRunDocRepo
  , appLogger
  , appUserHandle
  , appCsrfSecret
  , appSessionLength
  , AppState(..)
  , appCsrfToken
  , appUserState
  , AppUserState(..)
  , App(..)
  , AppError(..)
  , appIO
  , db
  , docRepo
  , appLog
  ) where

import Control.Lens (makeLenses, view)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Natural
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.Types
import Refine.Backend.User.Core
import Refine.Common.Types.Prelude (ID(..))
import Refine.Common.Types.User as Types (User)
import Refine.Common.VDoc.HTML (ChunkRangeError(..))
import Refine.Prelude (leftToError, Timespan)
import Refine.Prelude.TH (makeRefineType)

type RunDocRepo = DocRepo :~> ExceptT DocRepoError IO

data AppContext db = AppContext
  { _appRunDB         :: RunDB db
  , _appRunDocRepo    :: RunDocRepo
  , _appLogger        :: Logger
  , _appUserHandle    :: UserHandle
  , _appCsrfSecret    :: CsrfSecret
  , _appSessionLength :: Timespan
  }

data AppState = AppState
  { _appCsrfToken :: Maybe CsrfToken
  , _appUserState :: AppUserState
  }

-- | The state of the application depends on the user state.
data AppUserState
  = UserLoggedIn (ID Types.User) UserSession
  | UserLoggedOut
  deriving (Eq, Show)

makeLenses ''AppContext
makeLenses ''AppState

-- | Application monad handles
-- * database connection
-- * event logging
-- TODO:
-- * user authentication (login)
-- * user authorization (groups)
-- * use one db connection in one run, commit the result at the end.
newtype App db a = App { unApp :: StateT AppState (ReaderT (AppContext db) (ExceptT AppError IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppContext db)
    , MonadError AppError
    , MonadState AppState
    )

data AppError
  = AppUnknownError ST
  | AppVDocError [ChunkRangeError]
  | AppDBError DBError
  | AppDocRepoError DocRepoError
  | AppUserNotFound ST
  | AppUserNotLoggedIn
  | AppUserCreationError CreateUserError
  | AppCsrfError ST
  | AppSessionError
  | AppSanityCheckError ST
  deriving (Show, Generic)

makeRefineType ''AppError

appIO :: IO a -> App db a
appIO = App . liftIO

db :: db a -> App db a
db m = App $ do
  mu <- user <$> gets (view appUserState)
  (Nat runDB) <- ($ DBContext mu) <$> view appRunDB
  r <- liftIO (runExceptT (runDB m))
  leftToError AppDBError r
  where
    user = \case
      UserLoggedOut     -> Nothing
      UserLoggedIn u _s -> Just u

docRepo :: DocRepo a -> App db a
docRepo m = App $ do
  (Nat runDRepo) <- view appRunDocRepo
  r <- liftIO (runExceptT (runDRepo m))
  leftToError AppDocRepoError r

appLog :: String -> App db ()
appLog msg = App $ do
  logger <- view appLogger
  liftIO $ unLogger logger msg
