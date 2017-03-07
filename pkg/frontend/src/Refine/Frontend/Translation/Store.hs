{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Translation.Store where

import Data.String.Conversions (cs)
import Data.Text.I18n

import Refine.Common.Types.Translation
import Refine.Frontend.Types


translationsUpdate :: RefineAction -> Translations -> Translations
translationsUpdate (ChangeTranslations l10) _ = newTranslations l10
translationsUpdate _                        t = t

newTranslations :: L10 -> Translations
newTranslations (L10 ld l) = cs . localize ld l . gettext . _unTKey
