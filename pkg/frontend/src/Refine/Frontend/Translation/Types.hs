{-# LANGUAGE RankNTypes #-}
module Refine.Frontend.Translation.Types where

import Data.String.Conversions (ST)
import React.Flux

import Refine.Common.Types.Translation (TKey(..))


type Translations = TKey -> ST

type TranslationsRE = TKey -> forall e . ReactElementM e ()

emptyTranslations :: Translations
emptyTranslations = _unTKey
