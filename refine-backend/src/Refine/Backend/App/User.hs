{-# LANGUAGE ViewPatterns #-}

module Refine.Backend.App.User where

import           Control.Lens ((^.), view)
import           Control.Monad (void)
import           Control.Monad.Except
import           Data.String.Conversions (ST, cs)
import           Data.Time.Clock (NominalDiffTime)

import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.Database.Core (DB)
import Refine.Backend.User.Core as Users


login :: ST -> ST -> App DB ()
login username (Users.PasswordPlain -> password) = do
  appLog "login"
  let sessionDuration = 1000 :: NominalDiffTime  -- FIXME: move this to
                                                 -- 'Refine.Backend.Config.Config' and store the
                                                 -- config passed to 'startServer' in the App state.
  userHandle <- view appUserHandle
  session <- maybe (throwError (AppUserNotFound username)) pure
             =<< appIO (Users.authUser userHandle username password sessionDuration)
  setUserSession (UserSession session)

logout :: App DB ()
logout = do
  session <- currentUserSession
  userHandle <- view appUserHandle
  void . appIO $ Users.destroySession userHandle (session ^. unUserSession)
  clearUserSession

createUser :: ST -> ST -> ST -> App DB ()
createUser name email password = void $ do
  appLog "createUser"
  userHandle <- view appUserHandle
  let user = Users.User
              { Users.u_name  = name
              , Users.u_email = email
              , Users.u_password = Users.makePassword (Users.PasswordPlain password)
              , Users.u_active = True
              }
  either (throwError . AppUserCreationError . cs . show) pure
    =<< appIO (Users.createUser userHandle user)
