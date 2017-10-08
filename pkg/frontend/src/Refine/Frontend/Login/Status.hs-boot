{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Login.Status where
#include "import_frontend.hs"

import Refine.Frontend.Login.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Types


onLoginClick :: CurrentUser (Lookup User) -> [GlobalAction]
