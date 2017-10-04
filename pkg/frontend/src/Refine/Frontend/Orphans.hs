{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Orphans where
#include "import_frontend.hs"

import System.IO.Unsafe (unsafePerformIO)

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

instance FromJSVal RawContent where fromJSVal = fromJSValViaJSON
instance ToJSVal RawContent where toJSVal = toJSValViaJSON

instance PFromJSVal RawContent where pFromJSVal = fromJustNote "instance PFromJSVal RawContent" . unsafePerformIO . fromJSVal
instance PToJSVal RawContent where pToJSVal = unsafePerformIO . toJSVal

instance FromJSVal SelectionState where fromJSVal = fromJSValViaJSON
instance ToJSVal SelectionState where toJSVal = toJSValViaJSON

instance PFromJSVal SelectionState where pFromJSVal = fromJustNote "instance PFromJSVal SelectionState" . unsafePerformIO . fromJSVal
instance PToJSVal SelectionState where pToJSVal = unsafePerformIO . toJSVal

fromJSValViaJSON :: FromJSON v => JSVal -> IO (Maybe v)
fromJSValViaJSON v = (>>= parseMaybe parseJSON) <$> fromJSVal v

toJSValViaJSON :: ToJSON v => v -> IO JSVal
toJSValViaJSON = toJSVal . toJSON
