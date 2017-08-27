{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.Types where
#include "import_backend.hs"

import qualified Web.Users.Types as Users


newtype UserSession = UserSession { _unUserSession :: Users.SessionId }
  deriving (Eq, Show)

newtype CsrfSecret = CsrfSecret { _csrfSecret :: ST }
  deriving (Eq, Show)

newtype CsrfToken = CsrfToken { _csrfToken :: ST }
  deriving (Eq, Show)

userSessionText :: UserSession -> ST
userSessionText = cs . Users.unSessionId . _unUserSession

makeLenses ''UserSession
makeLenses ''CsrfSecret
makeLenses ''CsrfToken
