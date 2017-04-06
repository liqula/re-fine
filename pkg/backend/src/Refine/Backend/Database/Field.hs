{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Field where

import Control.Monad ((<=<))
import Data.Aeson
import Data.Proxy
import Data.String.Conversions (LBS, ST, cs)
import Database.Persist
import Database.Persist.Sql

import Refine.Prelude (Timestamp(..))
import Refine.Backend.DocRepo.Core
import Refine.Common.Types.Prelude (UserInfo)
import Refine.Common.Types.Chunk
import Refine.Common.Types.Process
import Refine.Common.Types.Role (Role(..))
import Refine.Common.Types.VDoc


instance PersistField Timestamp where
  toPersistValue (Timestamp t) = toPersistValue t
  fromPersistValue         = fmap Timestamp . fromPersistValue

instance PersistFieldSql Timestamp where
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)


instance PersistField Title where
  toPersistValue (Title t) = toPersistValue t
  fromPersistValue         = fmap Title . fromPersistValue

instance PersistFieldSql Title where
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)


instance PersistField Abstract where
  toPersistValue (Abstract t) = toPersistValue t
  fromPersistValue            = fmap Abstract . fromPersistValue

instance PersistFieldSql Abstract where
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)


instance PersistField RepoHandle where
  toPersistValue (RepoHandle t) = toPersistValue t
  fromPersistValue              = fmap RepoHandle . fromPersistValue

instance PersistFieldSql RepoHandle where
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)


instance PersistField EditHandle where
  toPersistValue (EditHandle t) = toPersistValue t
  fromPersistValue              = fmap EditHandle . fromPersistValue

instance PersistFieldSql EditHandle where
  sqlType _ = sqlType (Proxy :: Proxy ST)

-- * JSON stored values

toPersistJSONValue :: (ToJSON a) => a -> PersistValue
toPersistJSONValue = toPersistValue . cs @LBS @ST . encode

fromPersistJSONValue :: (FromJSON a) => PersistValue -> Either ST a
fromPersistJSONValue = (either (Left . cs) Right . eitherDecode . cs) <=< fromPersistValue @ST

instance PersistField ChunkRange where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql ChunkRange where
  sqlType _ = sqlType (Proxy :: Proxy ST)

instance PersistField EditKind where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql EditKind where
  sqlType _ = sqlType (Proxy :: Proxy ST)

instance PersistField Role where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql Role where
  sqlType _ = sqlType (Proxy :: Proxy ST)

instance PersistField CollaborativeEditPhase where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql CollaborativeEditPhase where
  sqlType _ = sqlType (Proxy :: Proxy ST)

instance PersistField UserInfo where
  toPersistValue   = toPersistJSONValue
  fromPersistValue = fromPersistJSONValue

instance PersistFieldSql UserInfo where
  sqlType _ = sqlType (Proxy :: Proxy ST)

