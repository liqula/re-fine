module Refine.Common.VDoc where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP

import Refine.Common.Prelude


data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)

data Document = Document
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoVDoc = ProtoVDoc
  deriving (Eq, Ord, Show, Read, Generic)

data VDoc = VDoc
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocTitle = VDocTitle { unVDocTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocAbstract = VDocAbstract { unVDocAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data VDocInfo = VDocInfo
  deriving (Eq, Ord, Show, Read, Generic)

-- * prototypes

type instance Proto VDoc = ProtoVDoc

-- * lens

makeLenses ''ConflictResolution
makeLenses ''Document
makeLenses ''ProtoVDoc
makeLenses ''VDoc
makeLenses ''VDocAbstract
makeLenses ''VDocInfo
makeLenses ''VDocTitle

makePrisms ''ConflictResolution
makePrisms ''Document
makePrisms ''ProtoVDoc
makePrisms ''VDoc
makePrisms ''VDocAbstract
makePrisms ''VDocInfo
makePrisms ''VDocTitle

-- * SOP

instance SOP.Generic ConflictResolution
instance SOP.Generic Document
instance SOP.Generic ProtoVDoc
instance SOP.Generic VDoc
instance SOP.Generic VDocAbstract
instance SOP.Generic VDocInfo
instance SOP.Generic VDocTitle

instance SOP.HasDatatypeInfo ConflictResolution
instance SOP.HasDatatypeInfo Document
instance SOP.HasDatatypeInfo ProtoVDoc
instance SOP.HasDatatypeInfo VDoc
instance SOP.HasDatatypeInfo VDocAbstract
instance SOP.HasDatatypeInfo VDocInfo
instance SOP.HasDatatypeInfo VDocTitle

-- * NFData

instance NFData ConflictResolution where rnf = SOP.grnf
instance NFData Document where rnf = SOP.grnf
instance NFData ProtoVDoc where rnf = SOP.grnf
instance NFData VDoc where rnf = SOP.grnf
instance NFData VDocAbstract where rnf = SOP.grnf
instance NFData VDocInfo where rnf = SOP.grnf
instance NFData VDocTitle where rnf = SOP.grnf

-- * JSON

instance ToJSON ConflictResolution where toJSON = gtoJSONDef
instance ToJSON Document where toJSON = gtoJSONDef
instance ToJSON ProtoVDoc where toJSON = gtoJSONDef
instance ToJSON VDoc where toJSON = gtoJSONDef
instance ToJSON VDocAbstract where toJSON = gtoJSONDef
instance ToJSON VDocInfo where toJSON = gtoJSONDef
instance ToJSON VDocTitle where toJSON = gtoJSONDef

instance FromJSON ConflictResolution where parseJSON = gparseJSONDef
instance FromJSON Document where parseJSON = gparseJSONDef
instance FromJSON ProtoVDoc where parseJSON = gparseJSONDef
instance FromJSON VDoc where parseJSON = gparseJSONDef
instance FromJSON VDocAbstract where parseJSON = gparseJSONDef
instance FromJSON VDocInfo where parseJSON = gparseJSONDef
instance FromJSON VDocTitle where parseJSON = gparseJSONDef
