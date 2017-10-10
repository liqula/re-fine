{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Core
  ( MkDBNat
  , DBRunner(..)
  , DBConnection(..)
  , AppContext(..)
  , appDBConnection
  , appLogChan
  , appToClientChan
  , appCacheInvalidationChan
  , appConfig
  , AppState(..)
  , initialAppState
  , appUserState
  , appCacheState
  , appIAmGod
  , AppUserState(..)
  , App
  , AppIO
  , AppM(..)
  , AppError(..)
  , SmtpError(..)
  , tryApp, toApiError, createUserErrorToApiError
  , MonadApp
  , MonadAppDB(dbWithFilters), db, dbUsersCmd
  , MonadLog(appLog)
  , MonadCache(..)
  , MonadToClient(..)
  ) where
#include "import_backend.hs"

import qualified Web.Users.Types as Users
import qualified Web.Users.Persistent as Users

import {-# SOURCE #-} Refine.Backend.App.Smtp
import {-# SOURCE #-} Refine.Backend.App.Translation
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Types
import Refine.Common.Access
import Refine.Common.Types as Types


newtype UserHandleError
  = UserHandleUnknownError String  -- ^ FUTUREWORK: make this 'SomeException'?
  deriving (Eq, Generic, Show)

makeRefineType ''UserHandleError

deriving instance Generic Users.CreateUserError
makeRefineType ''Users.CreateUserError


data AppContext = AppContext
  { _appDBConnection           :: DBConnection
  , _appLogChan                :: LogChan
  , _appToClientChan           :: TChan ToClient
  , _appCacheInvalidationChan  :: TChan (Set CacheKey)
  , _appConfig                 :: Config
  }

data AppState = AppState
  { _appUserState  :: AppUserState
  , _appCacheState :: Set CacheKey
  , _appIAmGod     :: Bool
  }
  deriving (Eq, Show)

-- | The state of the application depends on the user state.
-- FIXME: move to common and merge with 'CurrentUser' from package frontend.
data AppUserState
  = UserLoggedIn (ID Types.User) UserSession  -- ^ (re. 'UserSession', see #432.)
  | UserLoggedOut
  deriving (Eq, Show)

initialAppState :: Config -> AppState
initialAppState = AppState UserLoggedOut mempty . view cfgAllAreGods

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
  , MonadSmtp app
  , MonadToClient app
  , MonadCache app
  , MonadAccess app
  , MonadI18n app
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
  | AppSessionInvalid
  | AppSanityCheckError ST
  | AppL10ParseErrors [ST]
  | AppUnauthorized Creds
  | AppMergeError (ID Edit) (ID Edit) (ID Edit) ST
  | AppRebaseError (ID Edit)
  | AppSmtpError SmtpError
  deriving (Eq, Show, Generic)

newtype SmtpError
  = SmtpError IOException
  deriving (Eq, Show, Generic)

tryApp :: (MonadLog m, MonadError AppError m) => m a -> m (Either ApiError a)
tryApp m = (Right <$> m) `catchError` (fmap Left . toApiError)

-- | Convert 'AppError' (internal to backend) to 'ApiError' (shared between backend and frontend).
-- This also takes care of logging the confidential part of 'AppError' on the server side before
-- discarding it.
toApiError :: AppError -> (Monad m, MonadLog m) => m ApiError
toApiError err = l err >> pure (c err)
  where
    l = \case  -- (log levels may need some tweaking)
      AppUnknownError _          -> appLog LogError $ "AppError: " <> show err
      AppVDocVersionError        -> appLog LogError $ "AppError: " <> show err
      AppDBError _               -> appLog LogError $ "AppError: " <> show err
      AppUserNotFound _          -> appLog LogInfo  $ "AppError: " <> show err
      AppUserNotLoggedIn         -> appLog LogInfo  $ "AppError: " <> show err
      AppUserCreationError _     -> appLog LogInfo  $ "AppError: " <> show err
      AppSessionInvalid          -> appLog LogInfo  $ "AppError: " <> show err
      AppSanityCheckError _      -> appLog LogError $ "AppError: " <> show err
      AppL10ParseErrors _        -> appLog LogError $ "AppError: " <> show err
      AppUnauthorized _          -> appLog LogInfo  $ "AppError: " <> show err
      AppMergeError _ _ _ _      -> appLog LogError $ "AppError: " <> show err
      AppRebaseError _           -> appLog LogError $ "AppError: " <> show err
      AppSmtpError _             -> appLog LogError $ "AppError: " <> show err

    c = \case
      AppUnknownError _          -> ApiUnknownError
      AppVDocVersionError        -> ApiVDocVersionError
      AppDBError e               -> ApiDBError $ dbErrorToApiError e
      AppUserNotFound e          -> ApiUserNotFound e
      AppUserNotLoggedIn         -> ApiUserNotLoggedIn
      AppUserCreationError e     -> ApiUserCreationError $ createUserErrorToApiError e
      AppSessionInvalid          -> ApiSessionInvalid
      AppSanityCheckError e      -> ApiSanityCheckError e
      AppL10ParseErrors e        -> ApiL10ParseErrors e
      AppUnauthorized info       -> ApiUnauthorized (cs $ show info)
      AppMergeError base e1 e2 s -> ApiMergeError $ cs (show (base, e1, e2)) <> ": " <> s
      AppRebaseError _           -> ApiRebaseError
      AppSmtpError _             -> ApiSmtpError

dbErrorToApiError :: DBError -> ApiErrorDB
dbErrorToApiError = \case
  DBUnknownError         s -> ApiDBUnknownError         s
  DBNotFound             s -> ApiDBNotFound             s
  DBNotUnique            s -> ApiDBNotUnique            s
  DBException            s -> ApiDBException            s
  DBUserNotLoggedIn        -> ApiDBUserNotLoggedIn
  DBMigrationParseErrors _ -> ApiDBMigrationParseErrors
  DBUnsafeMigration      _ -> ApiDBUnsafeMigration

-- | so we don't have to export backend types to the frontend.
createUserErrorToApiError :: Users.CreateUserError -> ApiErrorCreateUser
createUserErrorToApiError Users.InvalidPassword              = ApiErrorInvalidPassword
createUserErrorToApiError Users.UsernameAlreadyTaken         = ApiErrorUsernameAlreadyTaken
createUserErrorToApiError Users.EmailAlreadyTaken            = ApiErrorEmailAlreadyTaken
createUserErrorToApiError Users.UsernameAndEmailAlreadyTaken = ApiErrorUsernameAndEmailAlreadyTaken


-- * database

class Database (AppDB app) => MonadAppDB app where
  type family AppDB app :: * -> *
  dbWithFilters :: XFilters -> (AppDB app) a -> app a

instance Database db => MonadAppDB (AppM db) where
  type AppDB (AppM db) = db
  dbWithFilters fltrs m = AppM $ do
    ctx <- do
      let user = \case
            UserLoggedOut     -> Nothing
            UserLoggedIn u _s -> Just u
      mu <- user <$> gets (view appUserState)
      pure (DBContext mu fltrs)
    mkNatDB <- view _1
    conn    <- view (_2 . appDBConnection)
    let (NT dbNat) = mkNatDB conn ctx
    r   <- liftIO (try $ runExceptT (dbNat m))
    r'  <- leftToError (AppDBError . DBUnknownError . show @SomeException) r  -- catch (unexpected?) IO errors
    leftToError AppDBError r'  -- catch (expected) errors we throw ourselves

db :: (MonadAppDB app) => (AppDB app) a -> app a
db = dbWithFilters mempty

dbUsersCmd :: (Users.Persistent -> IO a) -> (MonadAppDB app) => app a
dbUsersCmd = db . runUsersCmd


-- * logging

class MonadLog app where
  appLog :: LogLevel -> String -> app ()

instance MonadLog (AppM db) where
  appLog level msg = AppM $ do
    chan <- view (_2 . appLogChan)
    liftIO . atomically $ writeTChan chan (level, msg)

instance MonadLog (ReaderT LogChan IO) where
  appLog level msg = do
    chan <- ask
    liftIO . atomically $ writeTChan chan (level, msg)


-- * sending messages to client

class MonadToClient app where
  sendToClient :: ToClient -> app ()

instance MonadToClient (AppM db) where
  sendToClient msg = do
    chan <- asks $ view appToClientChan
    liftIO (atomically (writeTChan chan msg))


-- * caching

class MonadCache app where
  -- send notification to central invalidation TChan about invalid items.
  invalidateCaches :: Set CacheKey -> app ()

  -- ask client to destroy all cache items and and update 'AppState'.
  clearCache :: app ()

  -- send fresh cache items to client and update 'AppState'.
  fetchCache :: Set CacheKey -> app ()


-- * lens/TH

deriveClasses [([''AppError, ''SmtpError], [''Lens'])]
