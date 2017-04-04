{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
    DBNat
  , UHNat
  , DocRepoNat
  , AppContext(..)
  , appDBNat
  , appDocRepoNat
  , appUHNat
  , appLogger
  , appCsrfSecret
  , appSessionLength
  , appPoFilesRoot
  , AppState(..)
  , appCsrfToken
  , appUserState
  , AppUserState(..)
  , App
  , AppM(..)
  , AppError(..)
  , MonadApp
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
import System.FilePath (FilePath)

import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.Types
import Refine.Backend.User
import Refine.Common.Types as Types
import Refine.Common.VDoc.HTML (ChunkRangeError(..))
import Refine.Prelude (leftToError, Timespan)
import Refine.Prelude.TH (makeRefineType)


type DocRepoNat = DocRepo :~> ExceptT DocRepoError IO

data AppContext db uh = AppContext
  { _appDBNat         :: DBNat db
  , _appDocRepoNat    :: DocRepoNat
  , _appUHNat         :: UHNat uh
  , _appLogger        :: Logger
  , _appCsrfSecret    :: CsrfSecret
  , _appSessionLength :: Timespan
  , _appPoFilesRoot   :: FilePath
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

type MonadApp db uh =
  ( MonadDatabase db
  , StoreProcessData db Aula
  , StoreProcessData db CollaborativeEdit
  , MonadUserHandle uh
  , GroupOf db Edit
  , ProcessOf db Edit
  , Functor db
  , Applicative db
  )

-- | The 'App' defines the final constraint set that the
-- App API should use. Scraps the boilerplate for the
-- Refine.Backend.App.* modules.
type App a = forall db uh . MonadApp db uh => AppM db uh a

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
  | AppL10ParseErrors [ST]
  | AppUnauthorized
  deriving (Show, Generic)

makeRefineType ''AppError

appIO :: IO a -> AppM db uh a
appIO = AppM . liftIO

dbFilter :: Filters -> db a -> AppM db uh a
dbFilter fltrs m = AppM $ do
  mu <- user <$> gets (view appUserState)
  (Nat dbNat) <- ($ DBContext mu fltrs) <$> view appDBNat
  r <- liftIO (runExceptT (dbNat m))
  leftToError AppDBError r
  where
    user = \case
      UserLoggedOut     -> Nothing
      UserLoggedIn u _s -> Just u

db :: db a -> AppM db uh a
db = dbFilter mempty

docRepo :: DocRepo a -> AppM db uh a
docRepo m = AppM $ do
  (Nat drepoNat) <- view appDocRepoNat
  r <- liftIO (runExceptT (drepoNat m))
  leftToError AppDocRepoError r

userHandle :: uh a -> AppM db uh a
userHandle m = AppM $ do
  (Nat runUserHandle) <- view appUHNat
  r <- liftIO (runExceptT (runUserHandle m))
  leftToError AppUserHandleError r

appLog :: String -> AppM db uh ()
appLog msg = AppM $ do
  logger <- view appLogger
  liftIO $ unLogger logger msg
