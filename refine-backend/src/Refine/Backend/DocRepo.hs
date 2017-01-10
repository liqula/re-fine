module Refine.Backend.DocRepo
  ( module Refine.Backend.DocRepo.Core
  , module Refine.Backend.DocRepo.Darcs
  , createRunRepo
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Natural

import Refine.Backend.DocRepo.Core
import Refine.Backend.DocRepo.Darcs


createRunRepo :: IO (DocRepo :~> ExceptT DocRepoError IO)
createRunRepo = pure $ Nat (wrapErrors . runExceptT . unDocRepo)
  where
    wrapErrors :: IO (Either DocRepoError a) -> ExceptT DocRepoError IO a
    wrapErrors m = do
      r <- liftIO (try m)
      either (throwError . DocRepoException) (either throwError pure) r
