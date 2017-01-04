module Refine.Common.VDoc where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP

import Refine.Common.Prelude



data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)

data Document = Document
  deriving (Eq, Ord, Show, Read, Generic)

data VDoc = VDoc
  deriving (Eq, Ord, Show, Read, Generic)

data VDocInfo = VDocInfo
  deriving (Eq, Ord, Show, Read, Generic)

data VDocTitleAndDesc = VDocTitleAndDesc
  deriving (Eq, Ord, Show, Read, Generic)

-- * lens

makeLenses ''ConflictResolution
makeLenses ''Document
makeLenses ''VDoc
makeLenses ''VDocInfo
makeLenses ''VDocTitleAndDesc

makePrisms ''ConflictResolution
makePrisms ''Document
makePrisms ''VDoc
makePrisms ''VDocInfo
makePrisms ''VDocTitleAndDesc

-- * SOP

instance SOP.Generic ConflictResolution
instance SOP.Generic Document
instance SOP.Generic VDoc
instance SOP.Generic VDocInfo
instance SOP.Generic VDocTitleAndDesc

instance SOP.HasDatatypeInfo ConflictResolution
instance SOP.HasDatatypeInfo Document
instance SOP.HasDatatypeInfo VDoc
instance SOP.HasDatatypeInfo VDocInfo
instance SOP.HasDatatypeInfo VDocTitleAndDesc

-- * NFData

instance NFData ConflictResolution where rnf = SOP.grnf
instance NFData Document where rnf = SOP.grnf
instance NFData VDoc where rnf = SOP.grnf
instance NFData VDocInfo where rnf = SOP.grnf
instance NFData VDocTitleAndDesc where rnf = SOP.grnf

-- * JSON

instance ToJSON ConflictResolution where toJSON = gtoJSONDef
instance ToJSON Document where toJSON = gtoJSONDef
instance ToJSON VDoc where toJSON = gtoJSONDef
instance ToJSON VDocInfo where toJSON = gtoJSONDef
instance ToJSON VDocTitleAndDesc where toJSON = gtoJSONDef

instance FromJSON ConflictResolution where parseJSON = gparseJSONDef
instance FromJSON Document where parseJSON = gparseJSONDef
instance FromJSON VDoc where parseJSON = gparseJSONDef
instance FromJSON VDocInfo where parseJSON = gparseJSONDef
instance FromJSON VDocTitleAndDesc where parseJSON = gparseJSONDef
