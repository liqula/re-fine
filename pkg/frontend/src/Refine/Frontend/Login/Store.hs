{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.Login.Store where

import Refine.Frontend.Prelude

import Refine.Frontend.Login.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


loginStateUpdate :: HasCallStack => GlobalAction -> LoginState -> LoginState
loginStateUpdate (ChangeCurrentUser user) = lsCurrentUser .~ user
loginStateUpdate _                        = id
