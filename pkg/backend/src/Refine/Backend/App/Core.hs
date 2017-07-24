{-# LANGUAGE NoImplicitPrelude          #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Core (
    MkDBNat
  , DBRunner(..)
  , DBConnection(..)
  , AppContext(..)
  , appMkDBNat
  , appDBConnection
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
  , userHandle
  , appLog
  ) where

import Refine.Backend.Prelude

import System.FilePath (FilePath)
import qualified Web.Users.Types as Users
import qualified Web.Users.Persistent as Users

import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Types
import Refine.Common.Types as Types
import Refine.Prelude (leftToError, Timespan)
import Refine.Prelude.TH (makeRefineType)


newtype UserHandleError
  = UserHandleUnknownError String  -- ^ FUTUREWORK: make this 'SomeException'?
  deriving (Eq, Generic, Show)

makeRefineType ''UserHandleError

deriving instance Generic Users.CreateUserError
makeRefineType ''Users.CreateUserError


data AppContext db = AppContext
  { _appMkDBNat       :: MkDBNat db
  , _appDBConnection  :: DBConnection
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
-- FIXME:
-- * user authentication (login)
-- * user authorization (groups)
-- * use one db connection in one run, commit the result at the end.
newtype AppM db a = AppM { unApp :: StateT AppState (ReaderT (AppContext db) (ExceptT AppError IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppContext db)
    , MonadError AppError
    , MonadState AppState
    )

type MonadApp db =
  ( MonadDatabase db
  , StoreProcessData db Aula
  , StoreProcessData db CollaborativeEdit
  , GroupOf db Edit
  , ProcessOf db Edit
  , Functor db
  , Applicative db
  )

-- | The 'App' defines the final constraint set that the
-- App API should use. Scraps the boilerplate for the
-- Refine.Backend.App.* modules.
type App a = forall db . MonadApp db => AppM db a

data AppError
  = AppUnknownError ST
  | AppVDocVersionError
  | AppDBError DBError
  | AppUserNotFound ST
  | AppUserNotLoggedIn
  | AppUserCreationError Users.CreateUserError
  | AppUserHandleError UserHandleError
  | AppCsrfError ST
  | AppSessionError
  | AppSanityCheckError ST
  | AppL10ParseErrors [ST]
  | AppUnauthorized
  | AppMergeError (ID Edit) (ID Edit) (ID Edit) ST
  | AppRebaseError (ID Edit)
  deriving (Show, Generic)

makeRefineType ''AppError

appIO :: IO a -> AppM db a
appIO = AppM . liftIO

dbWithFilters :: XFilters -> db a -> AppM db a
dbWithFilters fltrs m = AppM $ do
  mu      <- user <$> gets (view appUserState)
  mkNatDB <- view appMkDBNat
  conn    <- view appDBConnection
  let (NT dbNat) = mkNatDB conn (DBContext mu fltrs)
  r   <- liftIO (try $ runExceptT (dbNat m))
  r'  <- leftToError (AppDBError . DBUnknownError . show @SomeException) r  -- catch (unexpected?) IO errors
  leftToError AppDBError r'  -- catch (expected) errors we throw ourselves
  where
    user = \case
      UserLoggedOut     -> Nothing
      UserLoggedIn u _s -> Just u

db :: db a -> AppM db a
db = dbWithFilters mempty

userHandle :: Database db => (Users.Persistent -> IO a) -> AppM db a
userHandle = db . runUsersCmd

appLog :: String -> AppM db ()
appLog msg = AppM $ do
  logger <- view appLogger
  liftIO $ unLogger logger msg
