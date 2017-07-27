{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Field where

import Refine.Backend.Prelude

import           Control.Monad ((<=<))
import           Database.Persist.Sql

import Refine.Backend.Database.Types
import Refine.Common.Types.Prelude (UserInfo)
import Refine.Common.Types.Role (Role(..))
import Refine.Common.Types.Core


-- FUTUREWORK: is there a way to derive PersistFieldSql instances for newtypes so that they always
-- take the type wrapped in the newtype?


instance PersistField Timestamp where
  toPersistValue (Timestamp t) = toPersistValue t
  fromPersistValue             = fmap Timestamp . fromPersistValue

instance PersistFieldSql Timestamp where
  sqlType Proxy = sqlType (Proxy @ST)  -- FIXME: should be @@UTCTime@, but that breaks tests on sqlite.


instance PersistField VDocVersion where
  toPersistValue (VDocVersion t) = toPersistValue t
  fromPersistValue               = fmap VDocVersion . fromPersistValue

instance PersistFieldSql VDocVersion where
  sqlType Proxy = sqlType (Proxy @ST)


instance PersistField Title where
  toPersistValue (Title t) = toPersistValue t
  fromPersistValue         = fmap Title . fromPersistValue

instance PersistFieldSql Title where
  sqlType Proxy = sqlType (Proxy @ST)


instance PersistField Abstract where
  toPersistValue (Abstract t) = toPersistValue t
  fromPersistValue            = fmap Abstract . fromPersistValue

instance PersistFieldSql Abstract where
  sqlType Proxy = sqlType (Proxy @ST)


-- * JSON stored values

toPersistJSONValue :: (ToJSON a) => a -> PersistValue
toPersistJSONValue = toPersistValue . cs @LBS @ST . encode

fromPersistJSONValue :: (FromJSON a) => PersistValue -> Either ST a
fromPersistJSONValue = (either (Left . cs) Right . eitherDecode . cs) <=< fromPersistValue @ST

instance PersistField RangePosition where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql RangePosition where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField EditKind where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql EditKind where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField Role where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql Role where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField UserInfo where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql UserInfo where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField MetaInfoID where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql MetaInfoID where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField RawContentEdit where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql RawContentEdit where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField RangePositions where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql RangePositions where
  sqlType Proxy = sqlType (Proxy @ST)

instance PersistField DBVotes where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql DBVotes where
  sqlType Proxy = sqlType (Proxy @ST)
