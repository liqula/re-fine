{-# LANGUAGE ViewPatterns #-}

module Refine.Backend.App.User where

import           Control.Lens ((^.), view)
import           Control.Monad (void)
import           Control.Monad.Except
import           Data.String.Conversions (cs)
import           Data.Time.Clock (NominalDiffTime)

import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Types
import Refine.Backend.User.Core as Users
import Refine.Common.Types.User as Refine
import Refine.Prelude (monadError)


login :: Login -> App DB ()
login (Login username (Users.PasswordPlain -> password)) = do
  appLog "login"
  let sessionDuration = 1000 :: NominalDiffTime  -- FIXME: move this to
                                                 -- 'Refine.Backend.Config.Config' and store the
                                                 -- config passed to 'startServer' in the App state.
  userHandle <- view appUserHandle
  session <- maybe (throwError (AppUserNotFound username)) pure
             =<< appIO (Users.authUser userHandle username password sessionDuration)
  void . setUserSession . UserSession $ session

logout :: App DB ()
logout = do
  session <- currentUserSession
  userHandle <- view appUserHandle
  void . appIO $ Users.destroySession userHandle (session ^. unUserSession)
  clearUserSession

createUser :: CreateUser -> App DB Refine.User
createUser (CreateUser name email password) = do
  appLog "createUser"
  userHandle <- view appUserHandle
  let user = Users.User
              { Users.u_name  = name
              , Users.u_email = email
              , Users.u_password = Users.makePassword (Users.PasswordPlain password)
              , Users.u_active = True
              }
  loginId <- monadError (AppUserCreationError . cs . show)
               =<< appIO (Users.createUser userHandle user)
  pure . Refine.User . Users.toUserID $ loginId
