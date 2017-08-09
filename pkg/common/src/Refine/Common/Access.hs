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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Access
  ( module Refine.Common.Access
  , module Refine.Common.Types.Role
  , NonEmpty((:|))
  ) where

import Refine.Common.Prelude

import Data.List.NonEmpty as NEL

import Refine.Common.Types.Core
import Refine.Common.Types.Prelude
import Refine.Common.Types.Role


-- | Any credentials we care about can be expressed as a value of this sum type.  Run 'hasCreds' to
-- find out if they are present.
data Cred =
    CredUser UserInfo
  | CredGroupRole GroupRole (ID Group)
  | CredGlobalRole GlobalRole
  deriving (Eq, Show)

data Creds =
    CredsLeaf Cred
  | CredsAll (NonEmpty Creds)
  | CredsAny (NonEmpty Creds)
  | CredsAlwaysAllow
  | CredsNeverAllow
  deriving (Eq, Show)

credsAll :: [Cred] -> Creds
credsAll xs = CredsAll $ CredsLeaf <$> NEL.fromList xs

credsAny :: [Cred] -> Creds
credsAny xs = CredsAny $ CredsLeaf <$> NEL.fromList xs


class (Functor m, Applicative m, Monad m) => MonadAccess m where
  hasCred :: Cred -> m Bool

hasCreds :: MonadAccess m => Creds -> m Bool
hasCreds (CredsLeaf x)        = hasCred x
hasCreds (CredsAll (x :| xs)) = and <$> (hasCreds `mapM` (x : xs))
hasCreds (CredsAny (x :| xs)) = or  <$> (hasCreds `mapM` (x : xs))
hasCreds CredsAlwaysAllow     = pure True
hasCreds CredsNeverAllow      = pure False
