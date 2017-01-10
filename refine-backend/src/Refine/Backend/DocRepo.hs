module Refine.Backend.DocRepo
  ( module Refine.Backend.DocRepo.Core
  , module Refine.Backend.DocRepo.Darcs
  , createRunRepo
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
import Control.Natural

import Refine.Backend.DocRepo.Core
import Refine.Backend.DocRepo.Darcs


createRunRepo :: FilePath -> IO (DocRepo :~> ExceptT DocRepoError IO)
createRunRepo repoDir = pure $
  Nat (wrapErrors . flip runReaderT (DocRepoCtx repoDir) . runExceptT . unDocRepo)
  where
    wrapErrors :: IO (Either DocRepoError a) -> ExceptT DocRepoError IO a
    wrapErrors m = do
      r <- liftIO (try m)
      either (throwError . DocRepoException) (either throwError pure) r
