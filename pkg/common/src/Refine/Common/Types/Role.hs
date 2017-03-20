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

module Refine.Common.Types.Role where

import Data.PartialOrd
import GHC.Generics

import Refine.Prelude.TH (makeRefineType)


-- | The 'Process' type determines the list of assignable roles (essentially just badges the user can
-- present to gain certain permissions) and what permissions the user gains by carrying what role.
-- This part can not only be changed in code.
--
-- Certain 'Role' constructors grant the power to assign 'Role's to other users (this happens at run
-- time in the UI).
--
-- TODO: #262.
data Role
  = ReadOnly
  | Member
  | Moderator
  | LocalAdmin
  | ProcessInitiator
  | GroupInitiator
  deriving (Enum, Eq, Ord, Show, Generic)

-- | Rights on operations, what to do with something
data Right
  = Create
  | Read
  | Update
  | Delete
  deriving (Eq, Show, Generic)

-- FIXME: Add more roles and define valid partial ordering.
-- | If a role A < role B, B overrules A.
instance PartialOrd Role where
  ReadOnly   <= ProcessInitiator = False
  ReadOnly   <= GroupInitiator   = False
  ReadOnly   <= _                = True

  Member     <= ProcessInitiator = False
  Member     <= GroupInitiator   = False
  Member     <= ReadOnly         = False
  Member     <= _                = True

  Moderator  <= ProcessInitiator = False
  Moderator  <= GroupInitiator   = False
  Moderator  <= ReadOnly         = False
  Moderator  <= Member           = False
  Moderator  <= _                = True

  LocalAdmin <= ProcessInitiator = False
  LocalAdmin <= GroupInitiator   = False
  LocalAdmin <= ReadOnly         = False
  LocalAdmin <= Member           = False
  LocalAdmin <= Moderator        = False
  LocalAdmin <= _                = True

  GroupInitiator <= GroupInitiator = True
  GroupInitiator <= _              = False

  ProcessInitiator <= ProcessInitiator = True
  ProcessInitiator <= _                = False

makeRefineType ''Role
makeRefineType ''Right
