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

module Refine.Backend.Types where

import Refine.Backend.Prelude

import Control.Lens (makeLenses)
import Data.String.Conversions (ST, cs)

import qualified Web.Users.Types as Users


newtype UserSession = UserSession { _unUserSession :: Users.SessionId }
  deriving (Eq, Show)

newtype CsrfSecret = CsrfSecret { _csrfSecret :: ST }
  deriving (Eq, Show)

newtype CsrfToken = CsrfToken { _csrfToken :: ST }
  deriving (Eq, Show)

userSessionText :: UserSession -> ST
userSessionText = cs . Users.unSessionId . _unUserSession

makeLenses ''UserSession
makeLenses ''CsrfSecret
makeLenses ''CsrfToken
