module Refine.Common.Patch where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP

import Refine.Common.Prelude



data Patch = Patch
  deriving (Eq, Ord, Show, Read, Generic)

data PatchFromClient = PatchFromClient
  deriving (Eq, Ord, Show, Read, Generic)

-- * lens

makeLenses ''Patch
makeLenses ''PatchFromClient

makePrisms ''Patch
makePrisms ''PatchFromClient

-- * SOP Generics

instance SOP.Generic Patch
instance SOP.Generic PatchFromClient

instance SOP.HasDatatypeInfo Patch
instance SOP.HasDatatypeInfo PatchFromClient

-- * NFData

instance NFData Patch where rnf = SOP.grnf
instance NFData PatchFromClient where rnf = SOP.grnf

-- * JSON

instance ToJSON Patch where toJSON = gtoJSONDef
instance ToJSON PatchFromClient where toJSON = gtoJSONDef

instance FromJSON Patch where parseJSON = gparseJSONDef
instance FromJSON PatchFromClient where parseJSON = gparseJSONDef
