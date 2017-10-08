{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Prelude.Aeson where

import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.String.Conversions
import           Data.Void
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)

newtype NoJSONRep a = NoJSONRep { _unNoJSONRep :: a }
  deriving (Eq, Functor, Generic)

instance Show (NoJSONRep a) where
  show _ = "NoJSONRep"

instance ToJSON (NoJSONRep a) where
  toJSON _ = toJSON ("NoJSONRep" :: ST)

instance FromJSON (NoJSONRep a) where
  -- only throw the error when the parsed value is forced.  this way, we have a chance to complete a
  -- parsed value, or just use the parts that were parseable.
  parseJSON (String "NoJSONRep") = pure . NoJSONRep $ error "NoJSONRep can be parsed, but not evaluated."
  parseJSON _ = fail "NoJSONRep"

instance NFData a => NFData (NoJSONRep a) where
  rnf (NoJSONRep a) = rnf a `seq` ()

instance SOP.Generic (NoJSONRep a)
instance SOP.HasDatatypeInfo (NoJSONRep a)

makeLenses ''NoJSONRep


(.=?) :: (KeyValue kv, ToJSON v) => ST -> Maybe v -> Maybe kv
(.=?) k (Just v) = Just $ k .= v
(.=?) _ Nothing  = Nothing

instance FromJSON Void where parseJSON _ = pure (error "parseJSON @Void")

instance ToJSON Void where toJSON = absurd
