{-# OPTIONS_GHC -fno-warn-orphans #-}
module Refine.Frontend.Orphans where

import Refine.Frontend.Prelude
import Refine.Common.Types

instance (FromJSVal a, FromJSVal b) => FromJSVal (Either a b)
instance (FromJSVal p) => FromJSVal (Range p)
instance (FromJSVal p) => FromJSVal (Selection p)
instance (FromJSVal block, FromJSVal col) => FromJSVal (GPosition block col)
instance FromJSVal SelectionState
instance FromJSVal BlockKey

instance (ToJSVal a, ToJSVal b) => ToJSVal (Either a b)
instance (ToJSVal p) => ToJSVal (Range p)
instance (ToJSVal p) => ToJSVal (Selection p)
instance (ToJSVal block, ToJSVal col) => ToJSVal (GPosition block col)
instance ToJSVal SelectionState
instance ToJSVal BlockKey
