{-# LANGUAGE ViewPatterns #-}

module Refine.Backend.App.User where

import           Control.Lens ((^.), view)
import           Control.Monad (void)
import           Control.Monad.State (gets)
import           Control.Monad.Reader (ask)
import           Data.Maybe (isJust)

import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Types
import Refine.Backend.User()
import Refine.Backend.User.Core  as Users
import Refine.Backend.User.Class as Users
import Refine.Common.Types      as Refine
import Refine.Prelude (nothingToError, leftToError, timespanToNominalDiffTime)


-- Username is returned after login. This turnes implicit user handling
-- to explicit one. The frontend code should use the returned username.
-- The rational here: It helps the future integration of different login
-- providers.
login :: Refine.Login -> App DB Username
login (Login username (Users.PasswordPlain -> password)) = do
  appLog "login"
  sessionDuration <- timespanToNominalDiffTime . view appSessionLength <$> ask
  session <- nothingToError (AppUserNotFound username)
             =<< userHandle (Users.authUser username password sessionDuration)
  loginId <- nothingToError AppSessionError
             =<< userHandle (Users.verifySession session)
  void $ setUserSession (toUserID loginId) (UserSession session)
  pure username

logout :: App DB ()
logout = do
  appLog "logout"
  st <- gets (view appUserState)
  case st of
    UserLoggedIn _user session -> do
      void . userHandle $ Users.destroySession (session ^. unUserSession)
      clearUserSession
    UserLoggedOut -> do
      pure ()

createUser :: CreateUser -> App DB Refine.User
createUser (CreateUser name email password) = do
  appLog "createUser"
  let user = Users.User
              { Users.u_name  = name
              , Users.u_email = email
              , Users.u_password = Users.makePassword (Users.PasswordPlain password)
              , Users.u_active = True
              }
  loginId <- leftToError AppUserCreationError
             =<< userHandle (Users.createUser user)
  pure . Refine.User . Users.toUserID $ loginId

doesUserExist :: ID Refine.User -> App DB Bool
doesUserExist uid = do
  isJust <$> userHandle (Users.getUserById (fromUserID uid))
