{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Types.Translation where

import Control.Lens (makeLenses)
import Data.String (IsString(..))
import Data.String.Conversions (ST, cs)
import GHC.Generics (Generic)
import Data.Text.I18n

import Refine.Prelude.Aeson()
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
