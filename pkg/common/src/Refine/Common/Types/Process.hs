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

import GHC.Generics

import Refine.Common.Types.Prelude (ID(..), Create)
import Refine.Prelude.TH (makeRefineType)


data CreateProcess a = CreateProcess
  { _createProcessData :: a
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

type instance Create (Process a) = CreateProcess a

makeRefineType ''CreateProcess
-- makeRefineType ''Process
