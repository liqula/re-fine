{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Field where

import Control.Monad ((<=<))
import Data.Aeson (encode, eitherDecode)
import Data.Proxy
import Data.String.Conversions (LBS, ST, cs)
import Database.Persist
import Database.Persist.Sql

import Refine.Backend.DocRepo.Core
import Refine.Common.Types.Chunk
import Refine.Common.Types.VDoc


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
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)

instance PersistField ChunkRange where
  toPersistValue range = toPersistValue . cs @LBS @ST $ encode range
  fromPersistValue = (either (Left . cs) Right . eitherDecode .  cs) <=< fromPersistValue @ST

instance PersistFieldSql ChunkRange where
  sqlType _ = sqlType (Proxy :: Proxy ST)
