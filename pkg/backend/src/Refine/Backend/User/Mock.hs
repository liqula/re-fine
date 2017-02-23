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

module Refine.Backend.User.Mock where

import Control.Monad.Free
import Data.Time (NominalDiffTime)
import Data.String.Conversions (ST)

import Refine.Backend.User.Class
import Refine.Backend.User.Core -- FIXME: Move to types.


-- FIXME: User Freer instead of Free
data FreeDB a where
  CreateUser     :: User    -> ((Either CreateUserError LoginId) -> a) -> FreeDB a
  GetUserById    :: LoginId -> ((Maybe User) -> a) -> FreeDB a

  AuthUser       :: ST -> PasswordPlain -> NominalDiffTime -> ((Maybe SessionId) -> a) -> FreeDB a
  VerifySession  :: SessionId -> ((Maybe LoginId) -> a) -> FreeDB a
  DestroySession :: SessionId -> (() -> a) -> FreeDB a


deriving instance Functor FreeDB


type Mock = Free FreeDB

createUser_ :: User -> Mock (Either CreateUserError LoginId)
createUser_ u = liftF $ CreateUser u id

getUserById_ :: LoginId -> Mock (Maybe User)
getUserById_ l = liftF $ GetUserById l id

authUser_ :: ST -> PasswordPlain -> NominalDiffTime -> Mock (Maybe SessionId)
authUser_ u p s = liftF $ AuthUser u p s id

verifySession_ :: SessionId -> Mock (Maybe LoginId)
verifySession_ s = liftF $ VerifySession s id

destroySession_ :: SessionId -> Mock ()
destroySession_ s = liftF $ DestroySession s id


instance UserHandle Mock where
  createUser     = createUser_
  getUserById    = getUserById_

  authUser       = authUser_
  verifySession  = verifySession_
  destroySession = destroySession_

data MockM m = MockM
  { mockCreateUser     :: User    -> m (Either CreateUserError LoginId)
  , mockGetUserById    :: LoginId -> m (Maybe User)
  , mockAuthUser       :: ST -> PasswordPlain -> NominalDiffTime -> m (Maybe SessionId)
  , mockVerifySession  :: SessionId -> m (Maybe LoginId)
  , mockDestroySession :: SessionId -> m ()
  }

interpret :: (Monad m) => MockM m -> Mock a -> m a
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
