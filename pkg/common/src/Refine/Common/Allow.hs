{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}

module Refine.Common.Allow where

import Refine.Common.Types

-- TODO: Rename it to Allow
class CheckPerm process target where
  checkPerm
    :: Maybe (ID User)
    -> Process process
    -> proxy target
    -> Role
    -> [Perm]

instance CheckPerm CollaborativeEdit Edit where
  checkPerm _ _ _ = \case
    ReadOnly         -> [Read]
    Member           -> [Create, Read]
    Moderator        -> []
    LocalAdmin       -> []
    ProcessInitiator -> []
    GroupInitiator   -> []

