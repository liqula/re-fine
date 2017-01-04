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

data VDoc = VDoc
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocTitle = VDocTitle { unVDocTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocAbstract = VDocAbstract { unVDocAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data VDocInfo = VDocInfo
  deriving (Eq, Ord, Show, Read, Generic)


-- * lens

makeLenses ''ConflictResolution
makeLenses ''Document
makeLenses ''VDoc
makeLenses ''VDocInfo

makePrisms ''ConflictResolution
makePrisms ''Document
makePrisms ''VDoc
makePrisms ''VDocInfo

-- * SOP

instance SOP.Generic ConflictResolution
instance SOP.Generic Document
instance SOP.Generic VDoc
instance SOP.Generic VDocInfo

instance SOP.HasDatatypeInfo ConflictResolution
instance SOP.HasDatatypeInfo Document
instance SOP.HasDatatypeInfo VDoc
instance SOP.HasDatatypeInfo VDocInfo

-- * NFData

instance NFData ConflictResolution where rnf = SOP.grnf
instance NFData Document where rnf = SOP.grnf
instance NFData VDoc where rnf = SOP.grnf
instance NFData VDocInfo where rnf = SOP.grnf

-- * JSON

instance ToJSON ConflictResolution where toJSON = gtoJSONDef
instance ToJSON Document where toJSON = gtoJSONDef
instance ToJSON VDoc where toJSON = gtoJSONDef
instance ToJSON VDocInfo where toJSON = gtoJSONDef

instance FromJSON ConflictResolution where parseJSON = gparseJSONDef
instance FromJSON Document where parseJSON = gparseJSONDef
instance FromJSON VDoc where parseJSON = gparseJSONDef
instance FromJSON VDocInfo where parseJSON = gparseJSONDef
