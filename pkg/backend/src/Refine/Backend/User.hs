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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.User where

import Control.Natural
import Control.Monad.Except
import Control.Monad.Reader

import Refine.Backend.User.Core
import Refine.Backend.User.Impl as UH
import Refine.Backend.User.Class


runUH :: UserDB -> RunUH
runUH usersHandle = Nat runUH'
  where
    runUH' :: UH a -> ExceptT UserHandleError IO a
    runUH' = (`runReaderT` UserHandleContext usersHandle) . unUH

instance UserHandle UH where
  createUser     = UH.createUser
  getUserById    = UH.getUserById

  authUser       = UH.authUser
  verifySession  = UH.verifySession
  destroySession = UH.destroySession

