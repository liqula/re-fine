{-# LANGUAGE NoImplicitPrelude          #-}
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

-- | This module mostly exists to resolve an import cycle between "Refine.Common.Types.Role" and
-- "Refine.Common.Types.Group".
module Refine.Common.ChangeAPI where

import Refine.Common.Prelude

import Refine.Common.Types.Role
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude
import Refine.Common.Types.Process


data ChangeSubGroup
  = AddSubGroup { _csgParent :: ID Group, _csgChild :: ID Group }
  | RmSubGroup  { _csgParent :: ID Group, _csgChild :: ID Group }
  deriving (Eq, Generic, Show)

data ChangeRole
  = AssignRole   { _crGroupRef :: GroupRef, _crUser :: ID User, _crRole :: Role }
  | UnassignRole { _crGroupRef :: GroupRef, _crUser :: ID User, _crRole :: Role }
  deriving (Eq, Generic, Show)

data ChangeProcess
  = ChangeProcessCollaborativeEditPhase
    { _cpProcessEditID :: ID (Process CollaborativeEdit)
    , _cpCollabEdit    :: CreateCollabEditProcess
    }
  | ChangeProcessAulaClassName
    { _cpProcessAulaID :: ID (Process Aula)
    , _cpAula          :: CreateAulaProcess
    }
  deriving (Eq, Generic, Show)

-- * Refine types

makeRefineTypes [''ChangeSubGroup, ''ChangeRole, ''ChangeProcess]
