{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Not for production.  Use this module for fast edit cycles when developing new components in the
-- browser.  Or move old components here temporarily to play with them.  It's ok for this module to
-- look at bit disorganised for now.
module Refine.Frontend.Workbench where

import Refine.Frontend.Prelude

import           Web.HttpApiData (toUrlPiece)

import           Refine.Common.Types
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Prelude ()
import qualified Refine.Prelude.BuildInfo as BuildInfo

import           Language.Css.Build
import           Language.Css.Syntax
import           Refine.Frontend.Colors as Color
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Login.Component
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- * workbench component

workbench :: HasCallStack => View '[GlobalState]
workbench = mkView "Workbench" $ \_gs -> do
  br_ [] >> br_ [] >> br_ [] >> hr_ []

  -- ...

  br_ [] >> br_ [] >> br_ [] >> hr_ []
  pure ()

workbench_ :: HasCallStack => GlobalState -> ReactElementM eventHandler ()
workbench_ = view_ workbench "Workbench_"


draftBox_ :: HasCallStack => ReactElementM handler () -> ReactElementM handler ()
draftBox_ = div_ ["style" @@= styles]
  where
    styles = [ decl "border" [Ident "dashed", Ident "black", Ident "2px"]
             , decl "padding" [Px 30]
             , decl "margin" [Px 10]
             ]


-- * now let's play!

-- ...
