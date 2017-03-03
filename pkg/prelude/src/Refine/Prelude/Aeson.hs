{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Prelude.Aeson where

import Data.Aeson
import Data.Map


instance  {-# OVERLAPPABLE #-} (ToJSON k, ToJSON v) => ToJSON (Map k v) where
  toJSON = toJSON . toList

instance  {-# OVERLAPPABLE #-} (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
  parseJSON = fmap fromList . parseJSON
