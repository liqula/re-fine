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
  , RunUH
  , RunDocRepo
  , AppContext(..)
  , appRunDB
  , appRunDocRepo
  , appRunUH
  , appLogger
  , appCsrfSecret
  , appSessionLength
  , AppState(..)
  , appCsrfToken
  , appUserState
  , AppUserState(..)
  , App
  , AppM(..)
  , AppError(..)
  , appIO
  , db
  , docRepo
  , userHandle
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
import Refine.Backend.User.Class
import Refine.Common.Types.Prelude (ID(..))
import Refine.Common.Types.User as Types (User)
import Refine.Common.VDoc.HTML (ChunkRangeError(..))
import Refine.Prelude (leftToError, Timespan)
import Refine.Prelude.TH (makeRefineType)

type RunDocRepo = DocRepo :~> ExceptT DocRepoError IO

data AppContext db uh = AppContext
  { _appRunDB         :: RunDB db
  , _appRunDocRepo    :: RunDocRepo
  , _appRunUH         :: RunUH uh
  , _appLogger        :: Logger
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
newtype AppM db uh a = AppM { unApp :: StateT AppState (ReaderT (AppContext db uh) (ExceptT AppError IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppContext db uh)
    , MonadError AppError
    , MonadState AppState
    )

-- | The 'App' defines the final constraint set that the
-- App API should use. Scraps the boilerplate for the
-- Refine.Backend.App.* modules.
type App a =
  forall db uh . (DatabaseM db, UserHandleM uh)
  => AppM db uh a

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
  | AppUserHandleError UserHandleError
  deriving (Show, Generic)

makeRefineType ''AppError

appIO :: IO a -> AppM db uh a
appIO = AppM . liftIO

db :: db a -> AppM db uh a
db m = AppM $ do
  mu <- user <$> gets (view appUserState)
  (Nat runDB) <- ($ DBContext mu) <$> view appRunDB
  r <- liftIO (runExceptT (runDB m))
  leftToError AppDBError r
  where
    user = \case
      UserLoggedOut     -> Nothing
      UserLoggedIn u _s -> Just u

docRepo :: DocRepo a -> AppM db uh a
docRepo m = AppM $ do
  (Nat runDRepo) <- view appRunDocRepo
  r <- liftIO (runExceptT (runDRepo m))
  leftToError AppDocRepoError r

userHandle :: uh a -> AppM db uh a
userHandle m = AppM $ do
  (Nat runUH) <- view appRunUH
  r <- liftIO (runExceptT (runUH m))
  leftToError AppUserHandleError r

appLog :: String -> AppM db uh ()
appLog msg = AppM $ do
  logger <- view appLogger
  liftIO $ unLogger logger msg
