{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}

module Refine.Common.Allow where

import Refine.Common.Types


-- | For any given data type, specify what a given 'Role' allows a user to do to it.  This is
-- slightly more clumsy to call than simple functions like 'allowGroup', 'allowVDoc', but it gives
-- the type checker more structure to work with.
class Allow a where
  allow :: proxy a -> Role -> [Perm]

instance Allow Edit where
  allow _ = \case
    ReadOnly         -> [Read]
    Member           -> [Create, Read]
    Moderator        -> []
    LocalAdmin       -> []
    ProcessInitiator -> []
    GroupInitiator   -> []

instance Allow Group where
  allow _ = \case
    ReadOnly         -> [Read]
    Member           -> [Read]
    Moderator        -> [Read]
    LocalAdmin       -> [Read, Update]
    ProcessInitiator -> [Create, Read, Update, Delete]
    GroupInitiator   -> [Create, Read, Update, Delete]

instance Allow VDoc where
  allow _ = \case
    ReadOnly         -> [Read]
    Member           -> [Create, Read, Update, Delete]
    Moderator        -> [Create, Read, Update, Delete]
    LocalAdmin       -> [Create, Read, Update, Delete]
    ProcessInitiator -> [Create, Read, Update, Delete]
    GroupInitiator   -> [Create, Read, Update, Delete]

class CheckPerm process target where
  checkPerm
    :: Maybe (ID User)
    -> Process process
    -> proxy target
    -> [Perm]
    -> ProcessAction (Process process)
    -> Bool
