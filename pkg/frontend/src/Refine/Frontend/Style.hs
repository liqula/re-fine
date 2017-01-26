{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}


module Refine.Frontend.Style where

import           Data.Aeson ((.=), object)
import           Data.Aeson.Types (ToJSON, toJSON)
import           Data.String.Conversions


data Style where
  Style :: forall a. (ToJSON a) => ST -> a -> Style

instance ToJSON [Style] where
  toJSON = object . fmap (\(Style k v) -> k .= v)

