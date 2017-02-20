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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
module Refine.Frontend.Login.Types where

import GHC.Generics (Generic)

import Refine.Common.Types.User (Username)
import Refine.Prelude.TH (makeRefineType)


data CurrentUser =
    NotLoggedInUser
  | LoggedInUser Username
  deriving (Show, Generic)

newtype LoginState = LoginState
  { _lsCurrentUser :: CurrentUser
  }
  deriving (Show, Generic)

emptyLoginState :: LoginState
emptyLoginState = LoginState
  { _lsCurrentUser = NotLoggedInUser
  }

makeRefineType ''CurrentUser
makeRefineType ''LoginState
