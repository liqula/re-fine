module Refine.Common.VDoc where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Prelude
import Refine.Common.Patch
import Refine.Prelude.TH


data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)

data Document = Document
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoVDoc = ProtoVDoc
  { _protoVDocTitle :: ST
  , _protoVDocDesc  :: ST
  }
  deriving (Eq, Ord, Show, Read, Generic)

type RepoId = ST

data VDocRepository = VDocRepository
  { _vdocRepositoryId    :: ID VDocRepository
  , _vdocRepositoryName  :: ST
  , _vdocDarcsRepository :: RepoId
  }
  deriving (Eq, Ord, Show, Read, Generic)

data VDoc = VDoc
  { _vdocId           :: ID VDoc
  , _vdocTitle        :: ST
  , _vdocDescription  :: ST
  , _vdocRepository   :: ID VDocRepository
  , _vdocHeadPatch    :: ID Patch
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocTitle = VDocTitle { unVDocTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocAbstract = VDocAbstract { unVDocAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data VDocInfo = VDocInfo
  deriving (Eq, Ord, Show, Read, Generic)


-- * prototypes

type instance Proto VDoc = ProtoVDoc

makeRefineType ''ConflictResolution
makeRefineType ''Document
makeRefineType ''ProtoVDoc
makeRefineType ''VDocRepository
makeRefineType ''VDoc
makeRefineType ''VDocTitle
makeRefineType ''VDocAbstract
makeRefineType ''VDocInfo
