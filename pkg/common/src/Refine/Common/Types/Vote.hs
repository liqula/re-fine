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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Vote where

import Refine.Common.Prelude

import GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude.TH


data CreateVote = CreateVote
  deriving (Eq, Ord, Show, Read, Generic)

data Vote = Vote
  deriving (Eq, Ord, Show, Read, Generic)

data VoteValue = VoteValue
  deriving (Eq, Ord, Show, Read, Generic)


-- * create types

type instance Create Vote = CreateVote

makeRefineType ''CreateVote
makeRefineType ''Vote
makeRefineType ''VoteValue
