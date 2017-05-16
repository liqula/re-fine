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

module Refine.Backend.User.Free (
    FreeUH
  , freeUHNat
  , MockUH(..), MockUH_
  , mockLogin
  ) where

import Refine.Backend.Prelude

import Control.Monad.Free

import Refine.Backend.User.Class
import Refine.Backend.User.Core
import Refine.Common.Types (ID(ID), Username)


-- FUTUREWORK: use "Freer" instead of "Free"
data UHAPI a where
  CreateUser     :: User    -> (Either CreateUserError LoginId -> a) -> UHAPI a
  GetUserById    :: LoginId -> (Maybe User -> a) -> UHAPI a
  GetUserIdByName :: Username -> (Maybe LoginId -> a) -> UHAPI a

  AuthUser       :: ST -> PasswordPlain -> NominalDiffTime -> (Maybe SessionId -> a) -> UHAPI a
  VerifySession  :: SessionId -> (Maybe LoginId -> a) -> UHAPI a
  DestroySession :: SessionId -> (() -> a) -> UHAPI a


deriving instance Functor UHAPI

type FreeUH = Free UHAPI


createUser_ :: User -> FreeUH (Either CreateUserError LoginId)
createUser_ u = liftF $ CreateUser u id

getUserById_ :: LoginId -> FreeUH (Maybe User)
getUserById_ l = liftF $ GetUserById l id

getUserIdByName_ :: Username -> FreeUH (Maybe LoginId)
getUserIdByName_ u = liftF $ GetUserIdByName u id

authUser_ :: ST -> PasswordPlain -> NominalDiffTime -> FreeUH (Maybe SessionId)
authUser_ u p s = liftF $ AuthUser u p s id

verifySession_ :: SessionId -> FreeUH (Maybe LoginId)
verifySession_ s = liftF $ VerifySession s id

destroySession_ :: SessionId -> FreeUH ()
destroySession_ s = liftF $ DestroySession s id


instance UserHandle FreeUH where
  type UserHandleInit FreeUH = MockUH_

  uhNat           = freeUHNat

  createUser      = createUser_
  getUserById     = getUserById_
  getUserIdByName = getUserIdByName_

  authUser        = authUser_
  verifySession   = verifySession_
  destroySession  = destroySession_


interpret :: (Monad m) => MockUH m -> FreeUH a -> m a
interpret _ (Pure x) = pure x

interpret m (Free (CreateUser u k)) = do
  r <- mockCreateUser m u
  interpret m (k r)

interpret m (Free (GetUserById l k)) = do
  r <- mockGetUserById m l
  interpret m (k r)

interpret m (Free (GetUserIdByName l k)) = do
  r <- mockGetUserIdByName m l
  interpret m (k r)

interpret m (Free (AuthUser u p s k)) = do
  r <- mockAuthUser m u p s
  interpret m (k r)

interpret m (Free (VerifySession s k)) = do
  r <- mockVerifySession m s
  interpret m (k r)

interpret m (Free (DestroySession s k)) = do
  r <- mockDestroySession m s
  interpret m (k r)

freeUHNat :: MockUH_ -> UHNat FreeUH
freeUHNat m = NT (interpret m)


type MockUH_ = MockUH (ExceptT UserHandleError IO)

data MockUH m = MockUH
  { mockCreateUser      :: User    -> m (Either CreateUserError LoginId)
  , mockGetUserById     :: LoginId -> m (Maybe User)
  , mockGetUserIdByName :: Username -> m (Maybe LoginId)
  , mockAuthUser        :: ST -> PasswordPlain -> NominalDiffTime -> m (Maybe SessionId)
  , mockVerifySession   :: SessionId -> m (Maybe LoginId)
  , mockDestroySession  :: SessionId -> m ()
  }

mockLogin :: Monad m => MockUH m
mockLogin = MockUH
  { mockCreateUser      = \_u -> pure . Right $ fromUserID userId
  , mockGetUserById     = \_l -> pure . Just $ error "mockLogin: No user information available."
  , mockGetUserIdByName = \_u -> pure . Just $ fromUserID userId
  , mockAuthUser        = mockAuthUserImpl
  , mockVerifySession   = \_s -> pure . Just $ fromUserID userId
  , mockDestroySession  = \_s -> pure ()
  }
  where
    userId = ID 0
    mockAuthUserImpl _u "" _t = pure Nothing
    mockAuthUserImpl _u _p _t = pure . Just $ SessionId "mock-session"
