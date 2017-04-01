{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Refine.Backend.User.Class where

import Data.String.Conversions (ST)
import Data.Time (NominalDiffTime)
import Web.Users.Types (PasswordPlain, SessionId, User, CreateUserError)
import Web.Users.Persistent (LoginId)

import Refine.Backend.User.Core
import Refine.Common.Types.User (Username)


type MonadUserHandle uh = (Monad uh, UserHandle uh)

-- TODO: Align
class UserHandle uh where
  type family UserHandleInit uh

  uhNat          :: UserHandleInit uh -> UHNat uh

  createUser     :: User    -> uh (Either CreateUserError LoginId)
  getUserById    :: LoginId -> uh (Maybe User)
  getUserIdByName  :: Username -> uh (Maybe LoginId)

  authUser       :: ST -> PasswordPlain -> NominalDiffTime -> uh (Maybe SessionId)
  verifySession  :: SessionId -> uh (Maybe LoginId)
  destroySession :: SessionId -> uh ()
