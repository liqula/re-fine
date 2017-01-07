module Refine.Backend.Database.Core where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Database.Persist.Sqlite



data DBConfig
  = DBInMemory
  | DBOnDisk FilePath

type SQLiteM = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

newtype DB a = DB { unDB :: ExceptT DBError SQLiteM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DBError
    )

data DBError
  = DBError String
  | DBNotFound String
  | DBException SomeException

notFound :: String -> DB a
notFound = DB . throwError . DBNotFound

liftDB :: SQLiteM a -> DB a
liftDB = DB . lift