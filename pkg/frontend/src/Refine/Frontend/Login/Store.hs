{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.Login.Store where

import Refine.Frontend.Prelude

--import Refine.Common.Types (userID)
import Refine.Frontend.Login.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


loginStateUpdate :: HasCallStack => GlobalAction -> LoginState -> LoginState
loginStateUpdate (SetCurrentUser user) = lsCurrentUser .~ user -- FIXME: fmap (^. userID) user
loginStateUpdate _                     = id
