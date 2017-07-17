{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Orphans where

import Refine.Frontend.Prelude
import Refine.Common.Types


instance (FromJSVal a, FromJSVal b) => FromJSVal (Either a b)
instance (FromJSVal p) => FromJSVal (Range p)
instance (FromJSVal p) => FromJSVal (Selection p)
instance (FromJSVal block, FromJSVal col) => FromJSVal (GPosition block col)
instance FromJSVal BlockKey

instance (ToJSVal a, ToJSVal b) => ToJSVal (Either a b)
instance (ToJSVal p) => ToJSVal (Range p)
instance (ToJSVal p) => ToJSVal (Selection p)
instance (ToJSVal block, ToJSVal col) => ToJSVal (GPosition block col)
instance ToJSVal BlockKey

instance FromJSVal RawContent where
  fromJSVal v = (>>= parseMaybe parseJSON) <$> fromJSVal v

instance ToJSVal RawContent where
  toJSVal = toJSVal . toJSON

instance FromJSVal SelectionState where
  fromJSVal v = (>>= parseMaybe parseJSON) <$> fromJSVal v

instance ToJSVal SelectionState where
  toJSVal = toJSVal . toJSON
