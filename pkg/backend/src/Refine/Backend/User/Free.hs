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
  , UHMock(..)
  , runFreeUH
  ) where

import Control.Natural
import Control.Monad.Except
import Control.Monad.Free
import Data.Time (NominalDiffTime)
import Data.String.Conversions (ST)

import Refine.Backend.User.Class
import Refine.Backend.User.Core -- FIXME: Move to types.



-- FIXME: User Freer instead of Free
-- TODO: Rename to DBAPI
data UHAPI a where
  CreateUser     :: User    -> ((Either CreateUserError LoginId) -> a) -> UHAPI a
  GetUserById    :: LoginId -> ((Maybe User) -> a) -> UHAPI a

  AuthUser       :: ST -> PasswordPlain -> NominalDiffTime -> ((Maybe SessionId) -> a) -> UHAPI a
  VerifySession  :: SessionId -> ((Maybe LoginId) -> a) -> UHAPI a
  DestroySession :: SessionId -> (() -> a) -> UHAPI a


deriving instance Functor UHAPI

type FreeUH = Free UHAPI

createUser_ :: User -> FreeUH (Either CreateUserError LoginId)
createUser_ u = liftF $ CreateUser u id

getUserById_ :: LoginId -> FreeUH (Maybe User)
getUserById_ l = liftF $ GetUserById l id

authUser_ :: ST -> PasswordPlain -> NominalDiffTime -> FreeUH (Maybe SessionId)
authUser_ u p s = liftF $ AuthUser u p s id

verifySession_ :: SessionId -> FreeUH (Maybe LoginId)
verifySession_ s = liftF $ VerifySession s id

destroySession_ :: SessionId -> FreeUH ()
destroySession_ s = liftF $ DestroySession s id


instance UserHandle FreeUH where
  createUser     = createUser_
  getUserById    = getUserById_

  authUser       = authUser_
  verifySession  = verifySession_
  destroySession = destroySession_

data UHMock m = UHMock
  { mockCreateUser     :: User    -> m (Either CreateUserError LoginId)
  , mockGetUserById    :: LoginId -> m (Maybe User)
  , mockAuthUser       :: ST -> PasswordPlain -> NominalDiffTime -> m (Maybe SessionId)
  , mockVerifySession  :: SessionId -> m (Maybe LoginId)
  , mockDestroySession :: SessionId -> m ()
  }

interpret :: (Monad m) => UHMock m -> FreeUH a -> m a
interpret _ (Pure x) = pure x

interpret m (Free (CreateUser u k)) = do
  r <- mockCreateUser m u
  interpret m (k r)

interpret m (Free (GetUserById l k)) = do
  r <- mockGetUserById m l
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

runFreeUH :: UHMock (ExceptT UserHandleError IO) -> RunUH FreeUH
runFreeUH m = Nat (interpret m)

