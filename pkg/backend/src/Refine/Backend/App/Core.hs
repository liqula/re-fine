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
  , AppIO
  , AppM(..)
  , AppError(..)
  , SmtpError(..)
  , MonadApp
  , MonadAppDB(dbWithFilters), db, dbUsersCmd
  , MonadLog(appLog)
  ) where

import Refine.Backend.Prelude

import           Control.Exception
import           System.FilePath (FilePath)
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


data AppContext = AppContext
  { _appDBConnection  :: DBConnection
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
newtype AppM db a = AppM { unApp :: StateT AppState (ReaderT (MkDBNat db, AppContext) (ExceptT AppError IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError AppError
    , MonadState AppState
    , MonadIO
    )

instance MonadReader AppContext (AppM db) where
  ask = AppM . lift . asks $ view _2
  local _ _ = error "local modification of reader state not supported for AppM."

-- | Final constraint set that all application combinators are allowed to use.  Note that even
-- though 'AppM' *does* derive 'MonadIO' or 'MonadBaseControl', we don't expose that here.  If you
-- write a combinator that need 'IO' exposed, you need to add it to its type explicitly.
type MonadApp app =
  ( Functor app
  , Applicative app
  , Monad app
  , MonadReader AppContext app
  , MonadError AppError app
  , MonadState AppState app
  , MonadAppDB app
  , MonadLog app
  )

-- | Syntactic sugar for 'MonadApp'.
type App a = forall app. MonadApp app => app a

-- | Syntactic sugar for 'MonadApp', 'MonadIO'.
type AppIO a = forall app. (MonadApp app, MonadIO app) => app a


-- | FUTUREWORK: use prismatic errors like in <https://github.com/liqd/aula>.  (it will require some
-- shuffling of code to avoid cyclical module imports, but it will separate the different effects
-- much better.)
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
  | AppSmtpError SmtpError
  deriving (Show, Generic)

newtype SmtpError
  = SmtpError IOException
  deriving (Show, Generic)


-- * database

class Database (AppDB app) => MonadAppDB app where
  type family AppDB app :: * -> *
  dbWithFilters :: XFilters -> (AppDB app) a -> app a

instance Database db => MonadAppDB (AppM db) where
 type AppDB (AppM db) = db
 dbWithFilters fltrs m = AppM $ do  -- TODO: indentation
  mu      <- user <$> gets (view appUserState)
  mkNatDB <- view _1
  conn    <- view (_2 . appDBConnection)
  let (NT dbNat) = mkNatDB conn (DBContext mu fltrs)
  r   <- liftIO (try $ runExceptT (dbNat m))
  r'  <- leftToError (AppDBError . DBUnknownError . show @SomeException) r  -- catch (unexpected?) IO errors
  leftToError AppDBError r'  -- catch (expected) errors we throw ourselves
  where
    user = \case
      UserLoggedOut     -> Nothing
      UserLoggedIn u _s -> Just u

db :: (MonadAppDB app) => (AppDB app) a -> app a
db = dbWithFilters mempty

dbUsersCmd :: (Users.Persistent -> IO a) -> (MonadAppDB app) => app a
dbUsersCmd = db . runUsersCmd


-- * logging

class MonadLog app where
  appLog :: String -> app ()

instance MonadLog (AppM db) where
  appLog msg = AppM $ do
    logger <- view (_2 . appLogger)
    liftIO $ unLogger logger msg


-- * lens/TH

deriveClasses [([''AppError, ''SmtpError], [''Lens'])]
