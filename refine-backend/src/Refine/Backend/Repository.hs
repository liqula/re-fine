module Refine.Backend.Repository
  ( module Refine.Backend.Repository.Core
  , module Refine.Backend.Repository.Darcs
  , createRunRepo
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Natural

import Refine.Backend.Repository.Core
import Refine.Backend.Repository.Darcs


createRunRepo :: IO (Repo :~> ExceptT RepoError IO)
createRunRepo = do

  pure $ Nat (wrapErrors . runExceptT. unRepo)
  where
    wrapErrors :: IO (Either RepoError a) -> ExceptT RepoError IO a
    wrapErrors m = do
      r <- liftIO (try m)
      either (throwError . RepoException) (either throwError pure) r
