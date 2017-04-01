{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Prelude.Aeson where

import           Control.Lens (makeLenses)
import           Control.DeepSeq
import           Data.Aeson
import           Data.String.Conversions
import qualified Generics.SOP as SOP
import           GHC.Generics

newtype NoJSONRep a = NoJSONRep { _unNoJSONRep :: a }
  deriving (Eq, Functor, Generic)

instance Show (NoJSONRep a) where
  show _ = "NoJSONRep"

instance ToJSON (NoJSONRep a) where
  toJSON _ = toJSON "NoJSONRep"

instance FromJSON (NoJSONRep a) where
  parseJSON = error "NoJSONRep has no json parser."

instance NFData a => NFData (NoJSONRep a) where
  rnf (NoJSONRep a) = rnf a `seq` ()

instance SOP.Generic (NoJSONRep a)
instance SOP.HasDatatypeInfo (NoJSONRep a)

makeLenses ''NoJSONRep


(.=?) :: (KeyValue kv, ToJSON v) => ST -> Maybe v -> Maybe kv
(.=?) k (Just v) = Just $ k .= v
(.=?) _ Nothing  = Nothing
