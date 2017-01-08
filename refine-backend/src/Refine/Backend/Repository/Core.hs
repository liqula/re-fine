module Refine.Backend.Repository.Core where

import Control.Lens (makeLenses)
import Control.Exception (SomeException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (ST)



data RepoError
  = RepoError String
  | RepoException SomeException
  deriving (Show)

-- TODO: Rename to DocRepo
newtype Repo a = Repo { unRepo :: ExceptT RepoError IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError RepoError
    )

repoIO :: IO a -> Repo a
repoIO = Repo . liftIO

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
