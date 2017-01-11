{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Field where

import Data.Proxy
import Data.String.Conversions (ST)
import Database.Persist
import Database.Persist.Sql

import Refine.Common.Types.VDoc
import Refine.Backend.DocRepo.Core


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


instance PersistField PatchHandle where
  toPersistValue (PatchHandle t) = toPersistValue t
  fromPersistValue               = fmap PatchHandle . fromPersistValue

instance PersistFieldSql PatchHandle where
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)
