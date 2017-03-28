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

makeRefineType ''CreateDBCollabEditProcess


type family ResultDB a = b | b -> a

data CollaborativeEditDB =
  CollaborativeEditDB
    { _collaborativeEditDBID    :: ID CollaborativeEdit
    , _collaborativeEditDBPhase :: CollaborativeEditPhase
    , _collaborativeEditDBVDoc  :: ID VDoc
    }
  deriving (Eq, Show, Generic)

type instance ResultDB (Process CollaborativeEdit) = CollaborativeEditDB
type instance ResultDB (Process Aula)              = Aula

makeRefineType ''CollaborativeEditDB
