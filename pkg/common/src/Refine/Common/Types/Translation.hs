{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-redundant-constraints #-}

module Refine.Common.Types.Translation where

import Control.Lens (Getter, makeLenses, to)
import Data.String.Conversions (ConvertibleStrings, ST, cs)
import Data.String (IsString(..))
import Data.Text.I18n
import GHC.Generics (Generic)

import Refine.Common.Orphans ()
import Refine.Prelude.Aeson (NoJSONRep(..))
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


type Translations = TKey -> ST

emptyTranslations :: Translations
emptyTranslations = _unTKey

type TranslationsCS = TKey -> forall s . ConvertibleStrings ST s => s


-- | See 'eqTrans'.
data Trans = Trans Int (NoJSONRep Translations)
  deriving (Show, Generic)

instance Eq Trans where
  (==) = eqTrans

emptyTrans :: Trans
emptyTrans = Trans 0 $ NoJSONRep emptyTranslations

updateTrans :: Trans -> Translations -> Trans
updateTrans (Trans i _) = Trans (i + 1) . NoJSONRep

-- | This is a bit of a hack, and needs to be used with care: the 'Eq' instance of 'Trans' uses this
-- to decide whether equality has changed.  This breaks when comparing two 'Trans' values with same
-- internal 'whoami' value, but different translation functions.  In order to keep that from
-- happening, only call 'emptyTrans' *once* in any program run, and then *always* call 'updateTrans'
-- on that value to create updates.
--
-- Note that if that is guaranteed, then 'Eq' may tell you two values are different if they are the
-- same.  This is not a big problem for our application (react needs this to decide whether to
-- re-render, so it may re-render without need, and without doing harm if it happens not too often).
eqTrans :: Trans -> Trans -> Bool
eqTrans (Trans i _) (Trans i' _) = i == i'

unTrans :: Getter Trans Translations
unTrans = to f  where f (Trans _ (NoJSONRep t)) = t


makeLenses ''TKey
makeRefineType ''Locale
makeRefineType ''Msgid
makeRefineType ''L10
makeRefineType ''GetTranslations
makeRefineType ''Trans  -- FIXME: Trans should be an abstract type.  we need to be more restrictive than that.
