module Refine.Backend.Database where

import Control.Natural
import Control.Monad.Except

import Lentil.Backend.SQLite.Core()



newtype DB a = DB { unDB :: ExceptT DBError IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DBError
    )

data DBError = DBError String

createDBRunner :: IO (DB :~> ExceptT DBError IO)
createDBRunner = do
  pure $ Nat unDB
