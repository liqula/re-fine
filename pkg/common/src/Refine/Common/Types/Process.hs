{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
module Refine.Common.Types.Process where

import Data.String.Conversions (ST)
import GHC.Generics

import Refine.Common.Types.Prelude (ID(..), Create)
import Refine.Common.Types.Group (Group)
import Refine.Common.Types.VDoc (VDoc)
import Refine.Prelude.TH (makeRefineType)


-- | TODO: Explain the error and why we can not do this...
--
-- data CreateProcess a = CreateProcess
--   { _createProcessData :: Create a
-- --  , _createProcessGRoup :: ID Group
--   }
--   deriving (Eq, Ord, Generic)
--
--    • No instance for (Eq (Create a))
--        arising from the first field of ‘CreateProcess’ (type ‘Create a’)
--      Possible fix:
--        use a standalone 'deriving instance' declaration,
--          so you can specify the instance context yourself
--    • When deriving the instance for (Eq (CreateProcess a))
--
data CreateCollabEditProcess = CreateCollabEditProcess
  { _createCollabEditProcessPhase   :: CollaborativeEditPhase
  , _createCollabEditProcessGroupID :: ID Group
  , _createCollabEditProcessVDocID  :: ID VDoc
  }
  deriving (Eq, Ord, Generic)

data CreateAulaProcess = CreateAulaProcess
  { _createAulaProcessClassName :: ST
  , _createAulaProcessGroupID   :: ID Group
  }
  deriving (Eq, Ord, Generic)


-- | A *process* has a certain type (more general: "collecting wild ideas",
-- "collaborative text editing", ...; or more specific: "spending a school budget", "updating a
-- party constitution", "setting up a Ltd.", ...) that determines much of what it looks like and how
-- it works.
--
-- Like groups, processes are initiated by users.  Unlike groups, a process always has exactly one
-- parent group ("its *home group*", or just "its group") and no children.
--
-- > data Process = Process (UID Process) MetaInfo Title Description (UID Group)
--
-- FIXME: not implemented yet.
data Process a = Process
  { _processID   :: ID (Process a)
  , _processData :: a
  }
  deriving (Eq, Ord, Generic)

type instance Create (Process CollaborativeEdit) = CreateCollabEditProcess
type instance Create (Process Aula)              = CreateAulaProcess

data CreateCollaborativeEdit = CreateCollaborativeEdit
  { _createCollaborativeEditPhase :: CollaborativeEditPhase
  }
  deriving (Eq, Ord, Generic)

data CollaborativeEdit =
  CollaborativeEdit
    { _collaborativeEditID    :: ID CollaborativeEdit
    , _collaborativeEditPhase :: CollaborativeEditPhase
    }
  deriving (Eq, Show, Generic)

data CollaborativeEditPhase = CollaborativeEditOnlyPhase  -- to be extended.
  deriving (Eq, Ord, Show, Generic)

-- FIXME: This is a placeholder item as an example of a different process type
-- This process type supports the database multitable implementation.
data Aula = Aula
  { _aulaID    :: ID Aula
  , _aulaClass :: ST
  }
  deriving (Eq, Show, Generic)

makeRefineType ''CreateCollabEditProcess
makeRefineType ''CreateAulaProcess
makeRefineType ''Process
makeRefineType ''CollaborativeEdit
makeRefineType ''CollaborativeEditPhase
makeRefineType ''Aula
