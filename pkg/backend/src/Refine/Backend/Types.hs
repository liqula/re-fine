{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Types where
#include "import_backend.hs"

import           Refine.Common.Types
import qualified Web.Users.Types as Users


type UserDetails = (Maybe ImageInline, ST)  -- ^ the part which is stored in our database

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
