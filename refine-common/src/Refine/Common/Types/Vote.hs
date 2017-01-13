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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Vote where

import GHC.Generics (Generic)

import Refine.Common.Types.Prelude
import Refine.Prelude.TH


data ProtoVote = ProtoVote
  deriving (Eq, Ord, Show, Read, Generic)

data Vote = Vote
  deriving (Eq, Ord, Show, Read, Generic)

data VoteValue = VoteValue
  deriving (Eq, Ord, Show, Read, Generic)


-- * prototype

type instance Proto Vote = ProtoVote

makeRefineType ''ProtoVote
makeRefineType ''Vote
makeRefineType ''VoteValue
