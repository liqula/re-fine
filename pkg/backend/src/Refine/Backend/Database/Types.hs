{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Refine.Backend.Database.Types where

import Refine.Backend.Prelude

import Data.Coerce (coerce)

import           Refine.Common.Types
import           Refine.Prelude.TH (makeRefineType)
import qualified Refine.Common.OT as OT


type family CreateDB a = b | b -> a

data CreateDBCollabEditProcess = CreateDBCollabEditProcess
  { _createDBCollabEditProcessPhase   :: CollaborativeEditPhase
  , _createDBCollabEditProcessGroupID :: ID Group
  , _createDBCollabEditProcessVDocID  :: ID VDoc
  }
  deriving (Eq, Show, Generic)

type instance CreateDB (Process CollaborativeEdit) = CreateDBCollabEditProcess
type instance CreateDB (Process Aula)              = CreateAulaProcess

-- | This type is used in 'MetaInfo' table.  We use the primary key from the tables for 'Note',
-- 'Question' etc., but those are not unique (each table has its own ID namespace).  'MetaInfoID'
-- makes the primary key unique for the 'MetaInfo' table.
--
-- (We could also just use @show . Data.Typeable.typeOf@ to the same end, but this way gives us a
-- comprehensive list of everything that can occur in the table.  If it gets to much work to
-- maintain this, we can refactor it to be more dynamic later.)
data MetaInfoID
  = MetaNote       (ID Note)
  | MetaQuestion   (ID Question)
  | MetaAnswer     (ID Answer)
  | MetaDiscussion (ID Discussion)
  | MetaStatement  (ID Statement)
  | MetaGroup      (ID Group)
  | MetaProcess    (ID (Process ()))  -- ^ It would be nice to have @(Process a)@ here, but not
                                      -- necessary, since all types of processes share the table,
                                      -- and thus the same ID namespace.
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

newtype RawContentEdit = RawContentEdit {unRawContentEdit :: OT.Edit RawContent}
  deriving (ToJSON, FromJSON, Monoid)

makeRefineType ''CreateDBCollabEditProcess
makeRefineType ''MetaInfoID
