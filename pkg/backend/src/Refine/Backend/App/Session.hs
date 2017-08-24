{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Session where

import Refine.Backend.Prelude

import Refine.Backend.App.Core
import Refine.Backend.Types
import Refine.Common.Types.Prelude (ID, User)


setUserSession :: ID User -> UserSession -> App ()
setUserSession user session = appUserState .= UserLoggedIn user session

currentUserSession :: App UserSession
currentUserSession = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut    -> throwError AppUserNotLoggedIn
    UserLoggedIn _ s -> pure s

clearUserSession :: App ()
clearUserSession = appUserState .= UserLoggedOut
