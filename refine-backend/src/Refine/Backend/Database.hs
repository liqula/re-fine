module Refine.Backend.Database
  ( module Refine.Backend.Database.Core
  , module Refine.Backend.Database.Entity
  , createDBRunner
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Natural
import Data.String.Conversions (cs)
import Database.Persist.Sqlite

import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema()
import Refine.Backend.Database.Entity



createDBRunner :: DBConfig -> IO (DB :~> ExceptT DBError IO)
createDBRunner cfg = do

  let sqliteDb = case cfg of
        DBInMemory  -> ":memory:"
        DBOnDisk fp -> fp

  let runQuery = wrapErrors . runSqlite (cs sqliteDb) . runExceptT

  pure $ Nat (runQuery . unDB)
  where
    wrapErrors :: IO (Either DBError a) -> ExceptT DBError IO a
    wrapErrors =
      lift . try >=> either (throwError . DBException) (either throwError pure)
