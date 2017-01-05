{-# LANGUAGE FlexibleContexts #-}
module Refine.Common.Prelude where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int
import Data.String.Conversions (cs)
import Data.Time
import Generics.SOP        as SOP
import Generics.SOP.JSON   as SOP
import Generics.SOP.NFData as SOP
import GHC.Generics        as GHC
import Text.Read
import Web.HttpApiData



newtype Timestamp = Timestamp { _unTimestamp :: UTCTime }
  deriving (Eq, Ord, Show, Read, GHC.Generic)

newtype ID a = ID { _unID :: Int64 }
  deriving (Eq, Ord, Show, Read, GHC.Generic)

type family Proto a :: *

gtoJSONDef :: forall a. (SOP.Generic a, HasDatatypeInfo a, All2 ToJSON (Code a)) =>  a -> Value
gtoJSONDef = SOP.gtoJSON SOP.defaultJsonOptions

gparseJSONDef :: forall a. (SOP.Generic a, HasDatatypeInfo a, All2 FromJSON (Code a)) => Value -> Parser a
gparseJSONDef = SOP.gparseJSON SOP.defaultJsonOptions

-- * lens

makeLenses ''Timestamp
makeLenses ''ID

makePrisms ''Timestamp
makePrisms ''ID

-- * SOP

instance SOP.Generic Timestamp
instance SOP.Generic (ID a)

instance SOP.HasDatatypeInfo Timestamp
instance SOP.HasDatatypeInfo (ID a)

-- * NFData

instance NFData Timestamp where rnf = SOP.grnf
instance NFData (ID a) where rnf = SOP.grnf

-- * JSON

instance ToJSON Timestamp where toJSON = gtoJSONDef
instance ToJSON (ID a) where toJSON = gtoJSONDef

instance FromJSON Timestamp where parseJSON = gparseJSONDef
instance FromJSON (ID a) where parseJSON = gparseJSONDef

-- * HttpApiData

instance ToHttpApiData (ID a) where
  toUrlPiece (ID x) = cs $ show x

instance FromHttpApiData (ID a) where
  parseUrlPiece = either (Left . cs) (Right . ID) . readEither @Int64 . cs
