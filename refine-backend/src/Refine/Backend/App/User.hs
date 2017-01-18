module Refine.Backend.App.User where

import Control.Lens ((^.), view)
import Control.Monad (void)
import Control.Monad.Except
import Data.String.Conversions (ST)
import Data.Time.Clock (NominalDiffTime)
import Web.Users.Types

import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.Database.Core (DB)


login :: ST -> PasswordPlain -> App DB ()
login username password = do
  appLog "login"
  userHandle <- view appUserHandle
  session <- maybe (throwError (AppUserNotFound username)) pure
             =<< appIO (authUser userHandle username password (1000 :: NominalDiffTime))
  setUserSession (UserSession session)

logout :: App DB ()
logout = do
  session <- currentUserSession
  userHandle <- view appUserHandle
  void . appIO $ destroySession userHandle (session ^. unUserSession)
  clearUserSession
