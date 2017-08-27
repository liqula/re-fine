{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
module Refine.Frontend.Login.Types where
#include "import_frontend.hs"

import GHC.Generics (Generic)


-- | FormError can be Nothing or Just an error string.
type FormError = Maybe ST

type CurrentUser = CurrentUser_ User

data CurrentUser_ user{-ID User | User-}
  = UserLoggedIn {_loggedInUser :: user}
  | UserLoggedOut
  deriving (Show, Eq, Generic, Functor)

newtype LoginState = LoginState
  { _lsCurrentUser :: CurrentUser  -- FIXME: CurrentUser_ (ID User)
  }
  deriving (Show, Eq, Generic)

emptyLoginState :: HasCallStack => LoginState
emptyLoginState = LoginState
  { _lsCurrentUser = UserLoggedOut
  }

makeRefineTypes [''CurrentUser_, ''LoginState]
