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

module Refine.Backend.App.Core (
    MkDBNat
  , DBRunner(..)
  , DBConnection(..)
  , UHNat
  , AppContext(..)
  , appMkDBNat
  , appDBConnection
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
  , userHandle
  , appLog
  ) where

import Refine.Backend.Prelude

import System.FilePath (FilePath)

import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Types
import Refine.Backend.User
import Refine.Common.Types as Types
import Refine.Prelude (leftToError, Timespan)
import Refine.Prelude.TH (makeRefineType)


data AppContext db uh = AppContext
  { _appMkDBNat       :: MkDBNat db
  , _appDBConnection  :: DBConnection
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
-- FIXME:
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
  | AppVDocVersionError
  | AppDBError DBError
  | AppUserNotFound ST
  | AppUserNotLoggedIn
  | AppUserCreationError CreateUserError
  | AppCsrfError ST
  | AppSessionError
  | AppSanityCheckError ST
  | AppUserHandleError UserHandleError
  | AppL10ParseErrors [ST]
  | AppUnauthorized
  | AppMergeError (ID Edit) (ID Edit) (ID Edit)
  | AppRebaseError (ID Edit)
  deriving (Show, Generic)

makeRefineType ''AppError

appIO :: IO a -> AppM db uh a
appIO = AppM . liftIO

dbWithFilters :: XFilters -> db a -> AppM db uh a
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

db :: db a -> AppM db uh a
db = dbWithFilters mempty

userHandle :: uh a -> AppM db uh a
userHandle m = AppM $ do
  (NT runUserHandle) <- view appUHNat
  r  <- liftIO (try $ runExceptT (runUserHandle m))
  r' <- leftToError (AppUserHandleError . UserHandleUnknownError . show @SomeException) r
  leftToError AppUserHandleError r'

appLog :: String -> AppM db uh ()
appLog msg = AppM $ do
  logger <- view appLogger
  liftIO $ unLogger logger msg
