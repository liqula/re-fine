{-# OPTIONS_GHC -fno-warn-orphans #-}
module Refine.Frontend.Orphans where

import Refine.Frontend.Prelude
import Refine.Common.Types

instance (FromJSVal a, FromJSVal b) => FromJSVal (Either a b)
instance FromJSVal SelectionState
instance FromJSVal SelectionPoint
instance FromJSVal BlockKey

instance (ToJSVal a, ToJSVal b) => ToJSVal (Either a b)
instance ToJSVal SelectionState
instance ToJSVal SelectionPoint
instance ToJSVal BlockKey
