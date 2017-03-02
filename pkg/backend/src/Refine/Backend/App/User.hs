{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.User where

import           Control.Lens ((^.), view)
import           Control.Monad (void)
import           Control.Monad.State (gets)
import           Control.Monad.Reader (ask)
import           Data.Maybe (isJust)

import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.Database.Class (DatabaseM)
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
login :: Refine.Login -> App Username
login (Login username (Users.PasswordPlain -> password)) = do
  appLog "login"
  sessionDuration <- timespanToNominalDiffTime . view appSessionLength <$> ask
  session <- nothingToError (AppUserNotFound username)
             =<< userHandle (Users.authUser username password sessionDuration)
  loginId <- nothingToError AppSessionError
             =<< userHandle (Users.verifySession session)
  void $ setUserSession (toUserID loginId) (UserSession session)
  pure username

logout :: App ()
logout = do
  appLog "logout"
  st <- gets (view appUserState)
  case st of
    UserLoggedIn _user session -> do
      void . userHandle $ Users.destroySession (session ^. unUserSession)
      clearUserSession
    UserLoggedOut -> do
      pure ()

createUser :: CreateUser -> App Refine.User
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

doesUserExist :: ID Refine.User -> App Bool
doesUserExist uid = do
  isJust <$> userHandle (Users.getUserById (fromUserID uid))

devModeUser :: Username
devModeUser = "dev"

devModePass :: Password
devModePass = "pass"

-- | Makes the dev user logs in to the system if the user is not logged in.
--
-- The user should be present in the database, or the user handling should
-- be mocked
devMode :: (UserHandleM uh, DatabaseM db) => AppM db uh a -> AppM db uh a
devMode m = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut    -> void . login $ Login devModeUser devModePass
    UserLoggedIn _ _ -> pure ()
  m
