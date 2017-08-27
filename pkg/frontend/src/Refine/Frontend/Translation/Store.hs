{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Translation.Store (translationsUpdate) where
#include "import_frontend.hs"

import Data.Text.I18n
import Refine.Common.Types.Translation
import Refine.Frontend.Store.Types


translationsUpdate :: HasCallStack => GlobalAction -> Trans -> Trans
translationsUpdate (ChangeTranslations l10) t = updateTrans t $ newTranslations l10
translationsUpdate _                        t = t

newTranslations :: HasCallStack => L10 -> Translations
newTranslations (L10 ld l) = cs . localize ld l . gettext . _unTKey
