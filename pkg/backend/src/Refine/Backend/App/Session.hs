module Refine.Backend.App.Session where

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)

import Refine.Backend.App.Core
import Refine.Backend.Types


setUserSession :: UserSession -> App db ()
setUserSession session = appUserState .= UserLoggedIn session

currentUserSession :: App db UserSession
currentUserSession = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut  -> throwError AppUserNotLoggedIn
    UserLoggedIn s -> pure s

clearUserSession :: App db ()
clearUserSession = appUserState .= UserLoggedOut
