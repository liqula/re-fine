module Refine.Backend.DocRepo.Core where

import GHC.Generics (Generic)
import Control.Lens (makeLenses)
import Control.Exception (SomeException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.String.Conversions (ST)



data DocRepoError
  = DocRepoUnknownError String
  | DocRepoException SomeException
  deriving (Show)

newtype DocRepoCtx = DocRepoCtx
  { _docRepoRoot :: FilePath
  }

newtype DocRepo a = DocRepo { unDocRepo :: ExceptT DocRepoError (ReaderT DocRepoCtx IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DocRepoError
    , MonadReader DocRepoCtx
    )

docRepoIO :: IO a -> DocRepo a
docRepoIO = DocRepo . liftIO

-- * types

newtype RepoHandle = RepoHandle { _unRepoHandle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype PatchHandle = PatchHandle { _unPatchHandle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

-- * lenses

makeLenses ''DocRepoCtx
makeLenses ''RepoHandle
makeLenses ''PatchHandle
