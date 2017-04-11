{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}

module Refine.Common.Allow where

import Refine.Common.Types


-- | FUTUREWORK: What we can't express with this is class is a role that takes away some permission
-- even if it was granted earlier.  We should wait until we need that, but one way to implement this
-- would be to have allow return two @[Perm]@, one to be added and one to be removed, and use the
-- 'Ord' instance of the 'Role' type to make @mconcat $ allow <$> (_ :: [Role])@ well-defined.
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
