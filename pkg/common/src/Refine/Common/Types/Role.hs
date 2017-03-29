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
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

-- | Rights on operations, what to do with something
data Perm
  = Create
  | Read
  | Update
  | Delete
  deriving (Eq, Show, Generic)

makeRefineType ''Role
makeRefineType ''Perm
