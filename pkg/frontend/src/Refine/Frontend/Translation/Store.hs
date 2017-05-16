{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Translation.Store (translationsUpdate) where

import Refine.Frontend.Prelude

import Data.String.Conversions (cs)
import Data.Text.I18n

import Refine.Common.Types.Translation
import Refine.Frontend.Store.Types


translationsUpdate :: GlobalAction -> Trans -> Trans
translationsUpdate (ChangeTranslations l10) t = updateTrans t $ newTranslations l10
translationsUpdate _                        t = t

newTranslations :: L10 -> Translations
newTranslations (L10 ld l) = cs . localize ld l . gettext . _unTKey
