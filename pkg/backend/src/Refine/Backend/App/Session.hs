module Refine.Backend.App.Session where

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)

import Refine.Backend.App.Core
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Types
import Refine.Backend.User.Core (UH)
import Refine.Common.Types.Prelude (ID)
import Refine.Common.Types.User (User)


setUserSession :: ID User -> UserSession -> App DB UH ()
setUserSession user session = appUserState .= UserLoggedIn user session

currentUserSession :: App DB UH UserSession
currentUserSession = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut    -> throwError AppUserNotLoggedIn
    UserLoggedIn _ s -> pure s

clearUserSession :: App DB UH ()
clearUserSession = appUserState .= UserLoggedOut
