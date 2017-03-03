{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Translation where

import Data.Text.I18n

import Refine.Common.Types.Translation
import Refine.Frontend.Types


translationsUpdate :: RefineAction -> Translations -> Translations
translationsUpdate (ChangeTranslations l10) _ = newTranslations l10
translationsUpdate _                        t = t

newTranslations :: L10 -> Translations
newTranslations (L10 ld) = localize ld (Locale "en_GB") . gettext
