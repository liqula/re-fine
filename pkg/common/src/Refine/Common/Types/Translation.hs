{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Types.Translation where

import Control.Lens (makeLenses)
import Data.String (IsString(..))
import Data.String.Conversions (ConvertibleStrings, ST, cs)
import GHC.Generics (Generic)
import Data.Text.I18n

import Refine.Common.Orphans ()
import Refine.Prelude.Aeson ()
import Refine.Prelude.TH (makeRefineType)


newtype GetTranslations = GetTranslations Locale
  deriving (Eq, Generic, Show)

data L10 = L10 L10n Locale
  deriving (Eq, Generic, Show)

deriving instance Generic Locale
deriving instance Generic Msgid


-- | Translation Key
newtype TKey = TKey { _unTKey :: ST }

instance IsString TKey where
  fromString = TKey . cs


makeLenses ''TKey
makeRefineType ''Locale
makeRefineType ''Msgid
makeRefineType ''L10
makeRefineType ''GetTranslations


type Translations = TKey -> ST

emptyTranslations :: Translations
emptyTranslations = _unTKey

type TranslationsCS = TKey -> forall s . ConvertibleStrings ST s => s
