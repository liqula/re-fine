{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Session where
#include "import_backend.hs"

import Refine.Backend.App.Core
import Refine.Backend.Types
import Refine.Common.Types.Prelude (ID, User)


setUserSession :: ID User -> UserSession -> App ()
setUserSession user session = appUserState .= UserLoggedIn user session

-- | This only deletes the 'App' session state, the state maintained by the users package is cleared
-- in 'Refine.Backend.App.User.logout'.
clearUserSession :: App ()
clearUserSession = appUserState .= UserLoggedOut
