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

import GHC.Generics

import Refine.Common.Types.Role
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude (ID)
import Refine.Common.Types.User
import Refine.Prelude.TH (makeRefineType)


data ChangeSubGroup
  = AddSubGroup { _csgParent :: ID Group, _csgChild :: ID Group }
  | RmSubGroup  { _csgParent :: ID Group, _csgChild :: ID Group }
  deriving (Eq, Generic, Show)

data ChangeRole
  = AssignRole   { _crGroup :: ID Group, _crUser :: ID User, _crRole :: Role }
  | UnassignRole { _crGroup :: ID Group, _crUser :: ID User, _crRole :: Role }
  deriving (Eq, Generic, Show)

-- * Refine types

makeRefineType ''ChangeSubGroup
makeRefineType ''ChangeRole
