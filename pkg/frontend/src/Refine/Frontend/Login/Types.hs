{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
module Refine.Frontend.Login.Types where
#include "import_frontend.hs"


-- | FormError can be Nothing or Just an error string.
type FormError = Maybe ST

data CurrentUser user{- User | ID User | Lookup User -}
  = UserLoggedIn {_loggedInUser :: user}
  | UserLoggedOut
  deriving (Show, Eq, Generic, Functor)

newtype LoginState = LoginState
  { _lsCurrentUser :: CurrentUser (ID User)
  }
  deriving (Show, Eq, Generic)

emptyLoginState :: HasCallStack => LoginState
emptyLoginState = LoginState
  { _lsCurrentUser = UserLoggedOut
  }

makeRefineTypes [''CurrentUser, ''LoginState]
