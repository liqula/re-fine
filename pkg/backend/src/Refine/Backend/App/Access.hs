{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Access where

import Refine.Backend.Prelude

import Refine.Backend.App.Core
import Refine.Backend.Config
import Refine.Backend.Database.Class as DB
import Refine.Common.Access
import Refine.Common.Types


instance Database db => MonadAccess (AppM db) where
  hasCred (CredUser uid)           = (uid ==) . maybe Anonymous UserID <$> pullUID
  hasCred CredNotLoggedIn          = (Nothing ==) <$> pullUID
  hasCred (CredGroupRole role gid) = (role `elem`) <$> pullGroupRolesIn gid
  hasCred (CredGlobalRole role)    = (role `elem`) <$> pullGobalRoles


pullUID :: (Monad m, MonadState AppState m) => m (Maybe (ID User))
pullUID = do
  st <- gets (view appUserState)
  pure $ case st of
    UserLoggedIn uid _ -> Just uid
    UserLoggedOut      -> Nothing

pullGroupRolesIn :: (Monad m, MonadState AppState m, MonadAppDB m) => ID Group -> m [GroupRole]
pullGroupRolesIn gid = maybe (pure []) (db . getGroupRolesIn gid) =<< pullUID

pullGobalRoles :: (Monad m, MonadState AppState m, MonadAppDB m) => m [GlobalRole]
pullGobalRoles = maybe (pure []) (db . getGlobalRoles) =<< pullUID


assertCred :: Cred -> (MonadError AppError m, MonadAccess m, MonadState AppState m, MonadLog m) => m ()
assertCred = assertCreds . CredsLeaf

assertCreds :: Creds -> (MonadError AppError m, MonadAccess m, MonadState AppState m, MonadLog m) => m ()
assertCreds xs = do
  a <- gets (view appIAmGod)
  b <- hasCreds xs
  appLogL LogDebug $ "assertCreds: " <> show (if a then "god" :: String else "mortal", xs, b)
  unless (a || b) . throwError $ AppUnauthorized (a, xs)

filterByCreds :: forall a m. (MonadAccess m, MonadState AppState m) => (a -> Creds) -> [a] -> m [a]
filterByCreds f as = do
  godMode <- gets (^. appIAmGod)
  if godMode then pure as else filterM (hasCreds . f) as


-- | Disable all authorization checks in 'AppState' temporarily
unsafeAsGod :: (Monad m, MonadState AppState m, MonadAppDB m, MonadLog m) => m a -> m a
unsafeAsGod action = do
  appLogL LogDebug ">>> [entering god mode]"
  modify (appIAmGod .~ True)
  result <- action
  modify (appIAmGod .~ False)
  appLogL LogDebug ">>> [left god mode]"
  pure result
