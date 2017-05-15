{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.Login.Store where

import Refine.Frontend.Prelude

import Control.Lens ((&), (.~))

import Refine.Frontend.Login.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


loginStateUpdate :: GlobalAction -> LoginState -> LoginState
loginStateUpdate (ChangeCurrentUser user) state = state & lsCurrentUser .~ user
loginStateUpdate _                        state = state
