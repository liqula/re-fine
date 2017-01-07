module Refine.Common.Vote where

import GHC.Generics (Generic)

import Refine.Common.Prelude
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
