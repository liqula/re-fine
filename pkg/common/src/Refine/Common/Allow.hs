{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}

module Refine.Common.Allow where

import Refine.Common.Types


class Allow process target where
  allow
    :: Maybe (ID User)
    -> Process process
    -> proxy target
    -> Role
    -> [Perm]

instance Allow CollaborativeEdit Edit where
  allow _ _ _ = \case
    ReadOnly         -> [Read]
    Member           -> [Create, Read]
    Moderator        -> []
    LocalAdmin       -> []
    ProcessInitiator -> []
    GroupInitiator   -> []

