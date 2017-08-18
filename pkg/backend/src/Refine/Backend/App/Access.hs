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


-- | Disable all authorization checks in 'AppState'.
unsafeBeAGod :: (Monad m, MonadState AppState m, MonadAppDB m, MonadLog m) => m ()
unsafeBeAGod = do
  appLogL LogDebug "*** unsafeBeAGod"
  modify (appIAmGod .~ True)

-- | Re-enable authorization checks after a call to 'unsafeBeAGod'.
beAMortal :: (Monad m, MonadState AppState m, MonadAppDB m, MonadLog m) => m ()
beAMortal = do
  appLogL LogDebug "*** beAMortal"
  modify (appIAmGod .~ False)
