{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Access
  ( module Refine.Common.Access
  , module Refine.Common.Types.Role
  , NonEmpty((:|))
  ) where
#include "import_common.hs"

import Refine.Common.Types.Core
import Refine.Common.Types.Prelude
import Refine.Common.Types.Role


-- | Any credentials we care about can be expressed as a value of this sum type.  Run 'hasCreds' to
-- find out if they are present.
data Cred =
    CredUser UserInfo
  | CredNotLoggedIn  -- ^ needed e.g. for registration form
  | CredGroupRole GroupRole (ID Group)
  | CredGlobalRole GlobalRole
  deriving (Eq, Show)

data Creds =
    CredsLeaf Cred
  | CredsAll (NonEmpty Creds)
  | CredsAny (NonEmpty Creds)
  | CredsAlwaysAllow
  | CredsNeverAllow
  deriving (Eq, Show)


class (Functor m, Applicative m, Monad m) => MonadAccess m where
  hasCred :: Cred -> m Bool

hasCreds :: MonadAccess m => Creds -> m Bool
hasCreds (CredsLeaf x)        = hasCred x
hasCreds (CredsAll (x :| xs)) = and <$> (hasCreds `mapM` (x : xs))
hasCreds (CredsAny (x :| xs)) = or  <$> (hasCreds `mapM` (x : xs))
hasCreds CredsAlwaysAllow     = pure True
hasCreds CredsNeverAllow      = pure False
