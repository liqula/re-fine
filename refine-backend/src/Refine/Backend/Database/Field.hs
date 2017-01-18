{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Field where

import Control.Monad ((<=<))
import Data.Aeson (encode, eitherDecode)
import Data.Proxy
import Data.String.Conversions (LBS, ST, cs)
import Database.Persist
import Database.Persist.Sql
import Text.Read (readEither)

import Refine.Common.Types.Chunk
import Refine.Common.Types.Note (NoteKind)
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

instance PersistField NoteKind where
  toPersistValue noteKind = toPersistValue @ST . cs $ show noteKind
  fromPersistValue        = (either (Left . cs) Right . readEither . cs) <=< fromPersistValue @ST

instance PersistFieldSql NoteKind where
  -- CAUTION: This should be generated, to represent the actual inner type
  -- of the title
  sqlType _ = sqlType (Proxy :: Proxy ST)

instance PersistField CreateChunkRange where
  toPersistValue range = toPersistValue . cs @LBS @ST $ encode range
  fromPersistValue = (either (Left . cs) Right . eitherDecode .  cs) <=< fromPersistValue @ST

instance PersistFieldSql CreateChunkRange where
  sqlType _ = sqlType (Proxy :: Proxy ST)
