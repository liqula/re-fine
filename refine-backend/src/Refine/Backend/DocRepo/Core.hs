module Refine.Backend.DocRepo.Core where

import Control.Lens (makeLenses)
import Control.Exception (SomeException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (ST)



data DocRepoError
  = DocRepoError String
  | DocRepoException SomeException
  deriving (Show)

newtype DocRepo a = DocRepo { unDocRepo :: ExceptT DocRepoError IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DocRepoError
    )

docRepoIO :: IO a -> DocRepo a
docRepoIO = DocRepo . liftIO

type RepoID     = ST
type CommitHash = ST

data Repository = Repository
  { _repoName :: ST
  , _repoId   :: RepoID
  }

newtype Commit = Commit
  { _commitHash :: CommitHash
  }

makeLenses ''Repository
makeLenses ''Commit
