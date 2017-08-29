-- | CAVEAT: https://ghc.haskell.org/trac/ghc/ticket/9562
module Refine.Backend.App.Translation where

import Data.Text.I18n.Po
import Refine.Common.Types.Translation

class MonadI18n app where
  getTranslations :: GetTranslations -> app L10
