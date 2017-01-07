module Refine.Common.VDoc where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Prelude
import Refine.Prelude.TH


data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)

data Document = Document
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoVDoc = ProtoVDoc
  deriving (Eq, Ord, Show, Read, Generic)

data VDoc = VDoc {
    _vdocTitle       :: ST
  , _vdocDescription :: ST
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
makeRefineType ''VDoc
makeRefineType ''VDocTitle
makeRefineType ''VDocAbstract
makeRefineType ''VDocInfo
