module Refine.Backend.DocRepo.Core where

import Control.Lens (makeLenses)
import Control.Exception (SomeException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (ST)



data DocRepoError
  = DocRepoUnknownError String
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

type RepoID  = ST
type PatchID = ST

data Repository = Repository
  { _repoName :: ST
  , _repoId   :: RepoID
  }

newtype Patch = Patch
  { _patchID :: PatchID
  }

makeLenses ''Repository
makeLenses ''Patch
