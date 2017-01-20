module Refine.Backend.App.Session where

import Control.Monad.Except (throwError)
import Control.Monad.State (put, get)

import Refine.Backend.App.Core


setUserSession :: UserSession -> App db ()
setUserSession = put . ActiveUser

currentUserSession :: App db UserSession
currentUserSession = do
  u <- get
  case u of
    NonActiveUser -> throwError AppUserNotLoggedIn
    ActiveUser s  -> pure s

clearUserSession :: App db ()
clearUserSession = put NonActiveUser
