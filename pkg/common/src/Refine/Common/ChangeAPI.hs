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
module Refine.Common.ChangeAPI where

import GHC.Generics

import Refine.Common.Types.Access
import Refine.Common.Types.Contribution (ContributionID)
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude (ID)
import Refine.Common.Types.User
import Refine.Prelude.TH (makeRefineType)


data ChangeAccess = ChangeAccess ContributionID Access (ID User)
  deriving (Eq, Show, Generic)

data ChangeSubGroup
  = AddSubGroup { _csgParent :: ID Group, _csgChild :: ID Group }
  | RmSubGroup  { _csgParent :: ID Group, _csgChild :: ID Group }
  deriving (Eq, Generic, Show)

data ChangeRole
  = AssignRole { _arGroup :: ID Group, _arUser :: ID User, _arRole :: Role }
  | UnassignRole { _arGroup :: ID Group, _arUser :: ID User, _arRole :: Role }
  deriving (Eq, Generic, Show)

-- * Refine types

makeRefineType ''ChangeAccess
makeRefineType ''ChangeSubGroup
makeRefineType ''ChangeRole
