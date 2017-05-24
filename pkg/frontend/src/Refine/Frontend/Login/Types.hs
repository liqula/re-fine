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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
module Refine.Frontend.Login.Types where

import Refine.Frontend.Prelude

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Common.Types.Prelude (Username)
import Refine.Prelude.TH (makeRefineType)


-- | FormError can be Nothing or Just an error string.
type FormError = Maybe ST

data CurrentUser
  = UserLoggedIn Username
  | UserLoggedOut
  deriving (Show, Eq, Generic)

instance UnoverlapAllEq CurrentUser

newtype LoginState = LoginState
  { _lsCurrentUser :: CurrentUser
  }
  deriving (Show, Eq, Generic)

emptyLoginState :: LoginState
emptyLoginState = LoginState
  { _lsCurrentUser = UserLoggedOut
  }

makeRefineType ''CurrentUser
makeRefineType ''LoginState
