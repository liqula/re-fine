{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Refine.Backend.Database.Types where

import GHC.Generics

import Refine.Common.Types
import Refine.Prelude.TH (makeRefineType)


type family CreateDB a = b | b -> a

data CreateDBCollabEditProcess = CreateDBCollabEditProcess
  { _createDBCollabEditProcessPhase   :: CollaborativeEditPhase
  , _createDBCollabEditProcessGroupID :: ID Group
  , _createDBCollabEditProcessVDocID  :: ID VDoc
  }
  deriving (Eq, Show, Generic)

type instance CreateDB (Process CollaborativeEdit) = CreateDBCollabEditProcess
type instance CreateDB (Process Aula)              = CreateAulaProcess

-- | This type tag is needed to make keys unique in MetaInfo table
data MetaInfoType
  = MetaNote
  | MetaQuestion
  | MetaAnswer
  | MetaDiscussion
  | MetaStatement
  | MetaGroup
  | MetaProcess
  | MetaVDoc
  | MetaEdit
  deriving (Eq, Show, Generic)

class HasMetaInfo a where
  metaInfoType :: ID a -> MetaInfoType

instance HasMetaInfo Note        where metaInfoType _ = MetaNote
instance HasMetaInfo Question    where metaInfoType _ = MetaQuestion
instance HasMetaInfo Answer      where metaInfoType _ = MetaAnswer
instance HasMetaInfo Discussion  where metaInfoType _ = MetaDiscussion
instance HasMetaInfo Statement   where metaInfoType _ = MetaStatement
instance HasMetaInfo Group       where metaInfoType _ = MetaGroup
instance HasMetaInfo (Process a) where metaInfoType _ = MetaProcess
instance HasMetaInfo VDoc        where metaInfoType _ = MetaVDoc
instance HasMetaInfo Edit        where metaInfoType _ = MetaEdit

makeRefineType ''CreateDBCollabEditProcess
makeRefineType ''MetaInfoType
