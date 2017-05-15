{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Refine.Backend.User.UH
  ( UH(..)
  , uhIO
  ) where

import Refine.Backend.Prelude

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
import Refine.Common.Types.Prelude (Username)


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

  uhNat db = NT ((`runReaderT` UserHandleContext db) . unUH)

  createUser      = Refine.Backend.User.UH.createUser
  getUserById     = Refine.Backend.User.UH.getUserById
  getUserIdByName = Refine.Backend.User.UH.getUserIdByName

  authUser        = Refine.Backend.User.UH.authUser
  verifySession   = Refine.Backend.User.UH.verifySession
  destroySession  = Refine.Backend.User.UH.destroySession


createUser :: User -> UH (Either CreateUserError LoginId)
createUser user = do
  ub <- view userBackend
  uhIO $ Users.createUser ub user

getUserById :: LoginId -> UH (Maybe User)
getUserById user = do
  ub <- view userBackend
  uhIO $ Users.getUserById ub user

getUserIdByName :: Username -> UH (Maybe LoginId)
getUserIdByName username = do
  ub <- view userBackend
  uhIO (Users.getUserIdByName ub username)

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
