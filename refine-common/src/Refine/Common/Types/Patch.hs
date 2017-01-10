module Refine.Common.Types.Patch where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)
import Refine.Prelude.TH
import Refine.Common.Prelude



-- FIXME: Wrap into a newtype
type CommitHash = ST

data Patch = Patch
  { _patchId          :: ID Patch
  , _patchDescription :: ST
  , _patchCommitHash  :: CommitHash
  }
  deriving (Eq, Ord, Show, Read, Generic)

data PatchFromClient = PatchFromClient
  deriving (Eq, Ord, Show, Read, Generic)

makeRefineType ''Patch
makeRefineType ''PatchFromClient
