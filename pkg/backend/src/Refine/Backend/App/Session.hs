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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Session where

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)

import Refine.Backend.App.Core
import Refine.Backend.Types
import Refine.Common.Types.Prelude (ID, User)


setUserSession :: ID User -> UserSession -> App ()
setUserSession user session = appUserState .= UserLoggedIn user session

currentUserSession :: App UserSession
currentUserSession = do
  u <- gets (view appUserState)
  case u of
    UserLoggedOut    -> throwError AppUserNotLoggedIn
    UserLoggedIn _ s -> pure s

clearUserSession :: App ()
clearUserSession = appUserState .= UserLoggedOut
