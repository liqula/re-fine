{-# LANGUAGE LambdaCase #-}

module Refine.Common.Allow where

import Data.Typeable (Proxy)

import Refine.Common.Types


-- | For any given data type, specify what a given 'Role' allows a user to do to it.  This is
-- slightly more clumsy to call than simple functions like 'allowGroup', 'allowVDoc', but it gives
-- the type checker more structure to work with.
class Allow a where
  allow :: Proxy a -> Role -> [Right]


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
