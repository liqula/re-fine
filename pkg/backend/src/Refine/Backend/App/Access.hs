{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Access where
#include "import_backend.hs"

import Refine.Backend.App.Core
import Refine.Backend.Config
import Refine.Backend.Database.Class as DB
import Refine.Common.Access
import Refine.Common.Types


instance Database db => MonadAccess (AppM db) where
  hasCred cred = do
    godMode <- gets (^. appIAmGod)
    if godMode then pure True else checkMortal cred
    where
      checkMortal (CredUser uid)           = (uid ==) . maybe Anonymous UserID <$> pullUID
      checkMortal CredNotLoggedIn          = (Nothing ==) <$> pullUID
      checkMortal (CredGroupRole role gid) = (role `elem`) <$> pullGroupRolesIn gid
      checkMortal (CredGlobalRole role)    = (role `elem`) <$> pullGobalRoles

  hasCreds creds = do
    godMode <- gets (^. appIAmGod)
    if godMode then pure True else defaultHasCreds creds


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
  ok <- hasCreds xs
  appLog LogDebug $ "assertCreds: " <> show (xs, ok)
  unless ok . throwError $ AppUnauthorized xs

filterByCreds :: forall a m. (MonadAccess m, MonadState AppState m) => (a -> Creds) -> [a] -> m [a]
filterByCreds f = filterM (hasCreds . f)


-- | Disable all authorization checks in 'AppState' temporarily
unsafeAsGod :: (Monad m, MonadState AppState m, MonadAppDB m, MonadLog m) => m a -> m a
unsafeAsGod action = do
  appLog LogDebug ">>> [entering god mode]"
  modify (appIAmGod .~ True)
  result <- action
  modify (appIAmGod .~ False)
  appLog LogDebug ">>> [left god mode]"
  pure result
