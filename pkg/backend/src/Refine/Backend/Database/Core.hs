{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Database.Core where
#include "import_backend.hs"

import Database.Persist.Sql hiding (Filter)

import Refine.Common.Types.Prelude (ID(..), User)


type SQLM = ReaderT SqlBackend IO

data DBContext = DBContext
  { _dbLoggedInUser :: Maybe (ID User)
  , _dbFilters      :: XFilters
  }

newtype DB a = DB { unDB :: ExceptT DBError (ReaderT DBContext SQLM) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DBError
    , MonadReader DBContext
    )

data DBError
  = DBUnknownError String  -- ^ FUTUREWORK: make this 'SomeException'?
  | DBNotFound String
  | DBNotUnique String
  | DBException String     -- ^ FUTUREWORK: make this 'SomeException'?
  | DBUserNotLoggedIn
  | DBMigrationParseErrors [ST]
  | DBUnsafeMigration [(Bool, ST)]
  deriving (Eq, Show, Generic)

-- FIXME: we probably want to eliminate that and store a current time value in 'DBContext' instead.
-- this will make all timestamps inside one transaction equal (and also slightly less accurate).
instance HasCurrentTime DB where
  getCurrentTimestamp = DB $ liftIO getCurrentTimestamp

-- | Filters the queries in the database.
-- Userful to implement pagination, uniqueness etc.
--
-- FIXME: In its current state this is not very practical, as it requires the output to be ordered
-- by default (which we probably never want), and it does not let you specify a page number.  The
-- next step (if we want pagination and not, say, filtering by full-text search), could be @data
-- Filter = Paginate PageNum PageLength | Sort@.
newtype XFilter = Limit Int

type XFilters = [XFilter]

makeRefineType ''DBError

makeLenses ''DBContext

notFound :: String -> DB a
notFound = DB . throwError . DBNotFound

notUnique :: String -> DB a
notUnique = DB . throwError . DBNotUnique

liftDB :: SQLM a -> DB a
liftDB = DB . lift . lift
