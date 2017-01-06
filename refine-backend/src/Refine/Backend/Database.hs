module Refine.Backend.Database where

import Control.Exception
import Control.Natural
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource

import Database.Persist.Sqlite



-- TODO: Add ExceptT DBError too
newtype DB a = DB { unDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    )

data DBError
  = DBError String
  | DBException SomeException

createDBRunner :: IO (DB :~> ExceptT DBError IO)
createDBRunner = do

  let runQuery q = wrapErrors $ runSqlite ":memory:" q

  pure $ Nat (runQuery . unDB)
  where
    wrapErrors :: IO a -> ExceptT DBError IO a
    wrapErrors =
      lift . try >=> either (throwError . DBException) pure
