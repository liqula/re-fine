{-# LANGUAGE NoImplicitPrelude          #-}
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
import Refine.Backend.Types
import Refine.Backend.User as User
import Refine.Backend.User.Core as User (User(..))
import Refine.Backend.Database.Class (createMetaID_)
import Refine.Common.Types as Refine
import Refine.Prelude (nothingToError, leftToError, timespanToNominalDiffTime)


-- Username is returned after login. This turnes implicit user handling
-- to explicit one. The frontend code should use the returned username.
-- The rational here: It helps the future integration of different login
-- providers.
login :: Refine.Login -> App Username
login (Login username (User.PasswordPlain -> password)) = do
  appLog "login"
  sessionDuration <- timespanToNominalDiffTime . view appSessionLength <$> ask
  session <- nothingToError (AppUserNotFound username)
             =<< userHandle (User.authUser username password sessionDuration)
  loginId <- nothingToError AppSessionError
             =<< userHandle (User.verifySession session)
  void $ setUserSession (toUserID loginId) (UserSession session)
  pure username

-- | Returns (Just (current ID)) of the current user if the user
-- is logged in otherwise Nothing.
currentUser :: App (Maybe (ID Refine.User))
currentUser = do
  st <- gets (view appUserState)
  pure $ case st of
    UserLoggedIn user _session -> Just user
    UserLoggedOut              -> Nothing

logout :: App ()
logout = do
  appLog "logout"
  st <- gets (view appUserState)
  case st of
    UserLoggedIn _user session -> do
      void . userHandle $ User.destroySession (session ^. unUserSession)
      clearUserSession
    UserLoggedOut -> do
      pure ()

createUser :: CreateUser -> App Refine.User
createUser (CreateUser name email password) = do
  appLog "createUser"
  let user = User.User
              { User.u_name  = name
              , User.u_email = email
              , User.u_password = User.makePassword (User.PasswordPlain password)
              , User.u_active = True
              }
  loginId <- leftToError AppUserCreationError
             =<< userHandle (User.createUser user)
  Refine.User <$> db (createMetaID_ $ User.toUserID loginId)

doesUserExist :: ID Refine.User -> App Bool
doesUserExist uid = do
  isJust <$> userHandle (User.getUserById (fromUserID uid))

devModeUser :: Username
devModeUser = "dev"

devModePass :: Password
devModePass = "pass"

-- | Makes the dev user logs in to the system if the user is not logged in.
--
-- The user should be present in the database, or the user handling should
-- be mocked
devMode :: MonadApp db uh => AppM db uh a -> AppM db uh a
devMode m = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut    -> void . login $ Login devModeUser devModePass
    UserLoggedIn _ _ -> pure ()
  m
