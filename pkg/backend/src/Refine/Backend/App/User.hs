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

import Refine.Backend.Prelude

import           Control.Lens ((^.), view)
import           Control.Monad (void)
import           Control.Monad.State (gets)
import           Control.Monad.Reader (ask)
import           Data.Maybe (isJust)
import qualified Web.Users.Types as Users

import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.Types
import Refine.Backend.Database.Class (createMetaID_, getMetaID)
import Refine.Backend.Database.Entity (toUserID, fromUserID)
import Refine.Common.Types as Common
import Refine.Prelude (nothingToError, leftToError, timespanToNominalDiffTime)


-- Username is returned after login. This turnes implicit user handling
-- to explicit one. The frontend code should use the returned username.
-- The rational here: It helps the future integration of different login
-- providers.
login :: Common.Login -> App Common.User
login (Login username (Users.PasswordPlain -> password)) = do
  appLog "login"
  sessionDuration <- timespanToNominalDiffTime . view appSessionLength <$> ask
  session <- nothingToError (AppUserNotFound username)
             =<< userHandle (\db_ -> Users.authUser db_ username password sessionDuration)
  loginId <- nothingToError AppSessionError
             =<< userHandle (\db_ -> Users.verifySession db_ session 0)
  void $ setUserSession (toUserID loginId) (UserSession session)
  user <- nothingToError (AppUserNotFound username) =<< userHandle (`Users.getUserById` loginId)
  mid <- db . getMetaID $ toUserID loginId
  pure $ Common.User mid username (Users.u_email user)

-- | Returns (Just (current ID)) of the current user if the user
-- is logged in otherwise Nothing.
currentUser :: App (Maybe (ID Common.User))
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
      void . userHandle $ \db_ -> Users.destroySession db_ (session ^. unUserSession)
      clearUserSession
    UserLoggedOut -> do
      pure ()

createUser :: CreateUser -> App Common.User
createUser (CreateUser name email password) = do
  appLog "createUser"
  let user = Users.User
              { Users.u_name  = name
              , Users.u_email = email
              , Users.u_password = Users.makePassword (Users.PasswordPlain password)
              , Users.u_active = True
              }
  loginId <- leftToError AppUserCreationError
             =<< userHandle (`Users.createUser` user)
  Common.User <$> db (createMetaID_ $ toUserID loginId) <*> pure name <*> pure email

doesUserExist :: ID Common.User -> App Bool
doesUserExist uid = do
  isJust <$> userHandle (\db_ -> Users.getUserById db_ (fromUserID uid))
