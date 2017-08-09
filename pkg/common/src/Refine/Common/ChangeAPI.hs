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
import Refine.Common.Types.Core
import Refine.Common.Types.Prelude


data ChangeSubGroup
  = AddSubGroup { _csgParent :: ID Group, _csgChild :: ID Group }
  | RmSubGroup  { _csgParent :: ID Group, _csgChild :: ID Group }
  deriving (Eq, Generic, Show)

data ChangeRole
  = AssignGroupRole    { _crUser :: ID User, _crGroupRole :: GroupRole, _crGroupRef :: ID Group }
  | UnassignGroupRole  { _crUser :: ID User, _crGroupRole :: GroupRole, _crGroupRef :: ID Group }
  | AssignGlobalRole   { _crUser :: ID User, _crGlobalRole :: GlobalRole }
  | UnassignGlobalRole { _crUser :: ID User, _crGlobalRole :: GlobalRole }
  deriving (Eq, Generic, Show)


makeRefineTypes [''ChangeSubGroup, ''ChangeRole]
