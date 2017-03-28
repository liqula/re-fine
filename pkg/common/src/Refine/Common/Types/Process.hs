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
import Refine.Common.Types.VDoc (VDoc, CompositeVDoc)
import Refine.Prelude.TH (makeRefineType)


-- | We can't do this, which would be more polymorphic:
--
-- data CreateProcess a = CreateProcess
--   { _createProcessData :: Create a
-- --  , _createProcessGRoup :: ID Group
--   }
--   deriving (Eq, Ord, Generic)
--
-- because ghc is not smart enough:
--
--    • No instance for (Eq (Create a))
--        arising from the first field of ‘CreateProcess’ (type ‘Create a’)
--      Possible fix:
--        use a standalone 'deriving instance' declaration,
--          so you can specify the instance context yourself
--    • When deriving the instance for (Eq (CreateProcess a))
--
data CreateCollabEditProcess = CreateCollabEditProcess
  { _createCollabEditProcessPhase :: CollaborativeEditPhase
  , _createCollabEditProcessGroup :: GroupRef
  , _createCollabEditProcessVDoc  :: Create VDoc
  }
  deriving (Eq, Show, Generic)

-- | 'CreateAulaProcess' is simple enough so we don't have to introduce an extra
-- 'CreateDBAulaProcess' that is a member of the 'CreateDB' type family in the backend.  This means
-- we cannot use 'UniveresalGroup' as a group reference.  We may want to change this in the future.
data CreateAulaProcess = CreateAulaProcess
  { _createAulaProcessClassName :: ST
  , _createAulaProcessGroupID   :: ID Group
  }
  deriving (Eq, Show, Generic)

-- | There is a special way to refer to a group called 'UniversalGroup', which is the root that
-- always exists.  'GroupRef' is a way to refer to either the universal group or some group that we
-- have the 'ID' of.
--
-- (Ultimately, we may not want to have this type around here, becasue there won't be a universal
-- group in which people do stuff in the portal once it has grown more mature.  For now it's a
-- useful and adequate abstraction.)
data GroupRef = GroupRef (ID Group) | UniversalGroup
  deriving (Eq, Show, Generic)

-- | A *process* has a certain type (more general: "collecting wild ideas",
-- "collaborative text editing", ...; or more specific: "spending a school budget", "updating a
-- party constitution", "setting up a Ltd.", ...) that determines much of what it looks like and how
-- it works.
--
-- Like groups, processes are initiated by users.  Unlike groups, a process always has exactly one
-- parent group ("its *home group*", or just "its group") and no children.
data Process a = Process
  { _processID    :: ID (Process a)
  , _processGroup :: Group
  , _processData  :: a
  }
  deriving (Eq, Show, Generic)

type instance Create (Process CollaborativeEdit) = CreateCollabEditProcess
type instance Create (Process Aula)              = CreateAulaProcess

data CollaborativeEdit =
  CollaborativeEdit
    { _collaborativeEditID    :: ID CollaborativeEdit
    , _collaborativeEditPhase :: CollaborativeEditPhase
    , _collaborativeEditVDoc  :: CompositeVDoc
    }
  deriving (Eq, Show, Generic)

data CollaborativeEditPhase = CollaborativeEditOnlyPhase  -- to be extended.
  deriving (Eq, Ord, Show, Generic)


-- | This is a placeholder item as an example of a different process type to demonstrate the
-- database multitable implementation.  FIXME: implement for real or remove.
data Aula = Aula
  { _aulaID    :: ID Aula
  , _aulaClass :: ST
  }
  deriving (Eq, Show, Generic)

-- | FIXME: rename to @CreateProcess@, instantiate 'Create' type family.  (or remove the latter, i
-- don't think it's used for anything any more.)
data AddProcess
  = AddCollabEditProcess CreateCollabEditProcess
  | AddAulaProcess       CreateAulaProcess
  deriving (Eq, Show, Generic)

data CreatedProcess
  = CreatedCollabEditProcess (Process CollaborativeEdit)
  | CreatedAulaProcess       (Process Aula)
  deriving (Eq, Show, Generic)

data RemoveProcess
  = RemoveCollabEditProcess (ID (Process CollaborativeEdit))
  | RemoveAulaProcess       (ID (Process Aula))
  deriving (Eq, Show, Generic)

makeRefineType ''GroupRef
makeRefineType ''CreateCollabEditProcess
makeRefineType ''CreateAulaProcess
makeRefineType ''Process
makeRefineType ''CollaborativeEdit
makeRefineType ''CollaborativeEditPhase
makeRefineType ''Aula
makeRefineType ''AddProcess
makeRefineType ''CreatedProcess
makeRefineType ''RemoveProcess
