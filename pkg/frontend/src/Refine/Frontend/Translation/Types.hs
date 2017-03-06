{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}

module Refine.Frontend.Translation.Types where

import Data.String.Conversions

import Refine.Common.Types.Translation (TKey(..))


type Translations = TKey -> ST

type TranslationsRE = TKey -> forall s . ConvertibleStrings ST s => s

emptyTranslations :: Translations
emptyTranslations = _unTKey
