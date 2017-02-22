module Refine.Backend.User.Impl where

import Control.Lens (view)
import Data.String.Conversions (ST)
import Data.Time (NominalDiffTime)
import Web.Users.Types      as Users
import Web.Users.Persistent as Users

import Refine.Backend.User.Core


createUser :: User -> UH (Either CreateUserError LoginId)
createUser user = do
  ub <- view userBackend
  uhIO $ Users.createUser ub user

getUserById :: LoginId -> UH (Maybe User)
getUserById user = do
  ub <- view userBackend
  uhIO $ Users.getUserById ub user

authUser :: ST -> PasswordPlain -> NominalDiffTime -> UH (Maybe SessionId)
authUser username password sessionDuration = do
  ub <- view userBackend
  uhIO $ Users.authUser ub username password sessionDuration

verifySession :: SessionId -> UH (Maybe LoginId)
verifySession session = do
  ub <- view userBackend
  uhIO $ Users.verifySession ub session 0

destroySession :: SessionId -> UH ()
destroySession session = do
  ub <- view userBackend
  uhIO $ Users.destroySession ub session
