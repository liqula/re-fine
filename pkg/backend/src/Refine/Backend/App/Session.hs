module Refine.Backend.App.Session where

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)

import Refine.Backend.App.Core
import Refine.Backend.Types
import Refine.Common.Types.Prelude (ID)
import Refine.Common.Types.User (User)


setUserSession :: ID User -> UserSession -> App db ()
setUserSession user session = appUserState .= UserLoggedIn user session

currentUserSession :: App db UserSession
currentUserSession = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut    -> throwError AppUserNotLoggedIn
    UserLoggedIn _ s -> pure s

clearUserSession :: App db ()
clearUserSession = appUserState .= UserLoggedOut
