module Refine.Common.Vote where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.NFData as SOP

import Refine.Common.Prelude


data Vote = Vote
  deriving (Eq, Ord, Show, Read, Generic)

data VoteValue = VoteValue
  deriving (Eq, Ord, Show, Read, Generic)

-- * lens

makeLenses ''Vote
makeLenses ''VoteValue

makePrisms ''Vote
makePrisms ''VoteValue

-- * SOP

instance SOP.Generic Vote
instance SOP.Generic VoteValue

instance SOP.HasDatatypeInfo Vote
instance SOP.HasDatatypeInfo VoteValue

-- * NFData

instance NFData Vote where rnf = SOP.grnf
instance NFData VoteValue where rnf = SOP.grnf

-- * JSON

instance ToJSON Vote where toJSON = gtoJSONDef
instance ToJSON VoteValue where toJSON = gtoJSONDef

instance FromJSON Vote where parseJSON = gparseJSONDef
instance FromJSON VoteValue where parseJSON = gparseJSONDef
