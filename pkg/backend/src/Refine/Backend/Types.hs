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

module Refine.Backend.Types where

import Control.Lens (makeLenses)
import Data.String.Conversions (ST, cs)

import Refine.Backend.User.Core (SessionId(..))


newtype UserSession = UserSession { _unUserSession :: SessionId }
  deriving (Eq, Show)

newtype CsrfSecret = CsrfSecret { _csrfSecret :: ST }
  deriving (Eq, Show)

newtype CsrfToken = CsrfToken { _csrfToken :: ST }
  deriving (Eq, Show)

userSessionText :: UserSession -> ST
userSessionText = cs . unSessionId . _unUserSession

makeLenses ''UserSession
makeLenses ''CsrfSecret
makeLenses ''CsrfToken
