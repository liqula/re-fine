module Refine.Common.Patch where

import GHC.Generics (Generic)

import Refine.Prelude.TH



data Patch = Patch
  deriving (Eq, Ord, Show, Read, Generic)

data PatchFromClient = PatchFromClient
  deriving (Eq, Ord, Show, Read, Generic)

makeRefineType ''Patch
makeRefineType ''PatchFromClient
