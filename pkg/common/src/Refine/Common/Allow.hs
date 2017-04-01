{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}

module Refine.Common.Allow where

import Refine.Common.Types


-- FUTUREWORK: Process target can be split up to Group and just
-- the process data. This abstraction can be extended to
-- handle other processes like administration group.
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

