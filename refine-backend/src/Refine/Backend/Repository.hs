module Refine.Backend.Repository where

import Control.Monad.Except
import Control.Natural

import Refine.Backend.Repository.Darcs()



data RepoError = RepoError String

newtype Repo a = Repo { unRepo :: ExceptT RepoError IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError RepoError
    )

createRepo :: IO (Repo :~> ExceptT RepoError IO)
createRepo = do
  pure $ Nat unRepo
