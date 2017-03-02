{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Refine.Backend.User.Impl
  ( UH(..)
  , uhIO
  ) where

import Control.Lens (view)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Natural
import Data.String.Conversions (ST)
import Data.Time (NominalDiffTime)
import Web.Users.Persistent as Users
import Web.Users.Types      as Users

import Refine.Backend.User.Core
import qualified Refine.Backend.User.Class as C


newtype UH a = UH { unUH :: ReaderT UserHandleContext (ExceptT UserHandleError IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader UserHandleContext
    )


uhIO :: IO a -> UH a
uhIO = UH . liftIO


instance C.UserHandle UH where
  type UserHandleInit UH = UserDB

  runUH db = Nat ((`runReaderT` UserHandleContext db) . unUH)

  createUser     = Refine.Backend.User.Impl.createUser
  getUserById    = Refine.Backend.User.Impl.getUserById

  authUser       = Refine.Backend.User.Impl.authUser
  verifySession  = Refine.Backend.User.Impl.verifySession
  destroySession = Refine.Backend.User.Impl.destroySession


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
