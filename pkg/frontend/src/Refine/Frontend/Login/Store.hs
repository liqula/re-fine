module Refine.Frontend.Login.Store where

import Control.Lens ((&), (.~))

import Refine.Frontend.Login.Types
import Refine.Frontend.Types (RefineAction(..))


loginStateUpdate :: RefineAction -> LoginState -> LoginState
loginStateUpdate (ChangeCurrentUser user) state = state & lsCurrentUser .~ user
loginStateUpdate _                        state = state
