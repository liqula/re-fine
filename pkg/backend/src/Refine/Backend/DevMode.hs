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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.DevMode where

import Data.Time (NominalDiffTime)
import Data.String.Conversions (ST)

import Refine.Backend.User.Free
import Refine.Backend.User
import Refine.Common.Types


mockLogin :: RunUH FreeUH
mockLogin = runFreeUH UHMock
  { mockCreateUser     = \_u -> pure . Right . fromUserID $ ID 0
  , mockGetUserById    = \_l -> pure . Just $ error "mockLogin: No user information is defined."
  , mockAuthUser       = mockedLogin
  , mockVerifySession  = \_s -> pure . Just . fromUserID $ ID 0
  , mockDestroySession = \_s -> pure ()
  }

mockedLogin :: Monad m => ST -> PasswordPlain -> NominalDiffTime -> m (Maybe SessionId)
mockedLogin _u "" _t = pure Nothing
mockedLogin _u _p _t = pure . Just $ SessionId "mock-session"
