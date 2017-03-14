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

module Refine.Common.Types.Access where

import GHC.Generics

import Refine.Prelude.TH (makeRefineType)


data Access = Grant | Revoke
  deriving (Eq, Show, Generic)

-- | Roles are fully ordered.
-- Eg: If a user is a moderator in a group, it also means it is a member
-- in that group.
data Role
  = ReadOnly
  | Member
  | Moderator
  | LocalAdmin
  | ProcessInitiator
  | GroupInitiator
  deriving (Eq, Show, Generic)

-- | Rights on operations, what to do with something
data Right
  = Create
  | Read
  | Update
  | Delete
  deriving (Eq, Ord, Show, Generic)

makeRefineType ''Access
makeRefineType ''Role
makeRefineType ''Right
