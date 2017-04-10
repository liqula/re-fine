{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Refine.Backend.Database.Types where

import GHC.Generics
import Data.Coerce (coerce)

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

-- | This type is used in MetaInfo table
data MetaInfoID
  = MetaNote       (ID Note)
  | MetaQuestion   (ID Question)
  | MetaAnswer     (ID Answer)
  | MetaDiscussion (ID Discussion)
  | MetaStatement  (ID Statement)
  | MetaGroup      (ID Group)
  | MetaProcess    (ID (Process ()))  -- ^ FIXME: use (Process a)
  | MetaUser       (ID User)
  | MetaVDoc       (ID VDoc)
  | MetaEdit       (ID Edit)
  deriving (Eq, Show, Generic)

class HasMetaInfo a where
  metaInfoType :: ID a -> MetaInfoID

instance HasMetaInfo Note        where metaInfoType = MetaNote
instance HasMetaInfo Question    where metaInfoType = MetaQuestion
instance HasMetaInfo Answer      where metaInfoType = MetaAnswer
instance HasMetaInfo Discussion  where metaInfoType = MetaDiscussion
instance HasMetaInfo Statement   where metaInfoType = MetaStatement
instance HasMetaInfo Group       where metaInfoType = MetaGroup
instance HasMetaInfo (Process a) where metaInfoType = MetaProcess . coerce
instance HasMetaInfo User        where metaInfoType = MetaUser
instance HasMetaInfo VDoc        where metaInfoType = MetaVDoc
instance HasMetaInfo Edit        where metaInfoType = MetaEdit

makeRefineType ''CreateDBCollabEditProcess
makeRefineType ''MetaInfoID
