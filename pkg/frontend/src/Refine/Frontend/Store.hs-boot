{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | CAVEAT: https://ghc.haskell.org/trac/ghc/ticket/9562
module Refine.Frontend.Store () where

import Refine.Frontend.Prelude
import Refine.Frontend.Store.Types
import Refine.Frontend.Util

instance Dispatchable GlobalAction
