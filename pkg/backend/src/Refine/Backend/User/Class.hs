{-# LANGUAGE ConstraintKinds #-}
module Refine.Backend.User.Class where

import Data.String.Conversions (ST)
import Data.Time (NominalDiffTime)
import Web.Users.Types (PasswordPlain, SessionId, User, CreateUserError)
import Web.Users.Persistent (LoginId)


type UserHandleM uh = (Monad uh, UserHandle uh)

class UserHandle uh where
  createUser     :: User    -> uh (Either CreateUserError LoginId)
  getUserById    :: LoginId -> uh (Maybe User)

  authUser       :: ST -> PasswordPlain -> NominalDiffTime -> uh (Maybe SessionId)
  verifySession  :: SessionId -> uh (Maybe LoginId)
  destroySession :: SessionId -> uh ()
