module Refine.Common.Types.VDoc where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude.TH


data VDoc = VDoc
  { _vdocId       :: ID VDoc
  , _vdocTitle    :: Title
  , _vdocAbstract :: Abstract
  , _vdocRepo     :: ID VDocRepo
  }
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoVDoc = ProtoVDoc
  { _protoVDocTitle       :: Title
  , _protoVDocAbstract    :: Abstract
  , _protoVDocInitVersion :: VDocVersion
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocVersion = VDocVersion { _unVDocVersion :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data VDocRepo = VDocRepo
  { _vdocRepoID    :: ID VDocRepo
  , _vdocHeadPatch :: ID Patch
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Patch = Patch
  { _patchId   :: ID Patch
  , _patchDesc :: ST
  }
  deriving (Eq, Ord, Show, Read, Generic)

data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)


-- * protos, instances

type instance Proto VDoc = ProtoVDoc

makeRefineType ''VDoc
makeRefineType ''ProtoVDoc
makeRefineType ''VDocRepo
makeRefineType ''Patch
makeRefineType ''ConflictResolution
makeRefineType ''Title
makeRefineType ''Abstract
makeRefineType ''VDocVersion
