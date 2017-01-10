module Refine.Backend.DocRepo.Core where

import GHC.Generics (Generic)
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


newtype RepoHandle = RepoHandle { _unRepoHandle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype PatchHandle = PatchHandle { _unPatchHandle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)


makeLenses ''RepoHandle
makeLenses ''PatchHandle
