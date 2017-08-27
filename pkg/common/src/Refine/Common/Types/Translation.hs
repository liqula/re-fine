{-# LANGUAGE CPP #-}
#include "language.hs"
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-redundant-constraints #-}

module Refine.Common.Types.Translation where
#include "import_common.hs"

import Control.Lens (Getter, makeLenses, to)
import Data.String (IsString(..))
import Data.Text.I18n as I18n (Locale(..), L10n, Msgid(..), Context)
import GHC.Generics (Generic)

import Refine.Prelude.Aeson (NoJSONRep(..))

newtype GetTranslations = GetTranslations Locale
  deriving (Eq, Generic, Show)

data L10 = L10 L10n Locale
  deriving (Eq, Generic, Show)

deriving instance Generic Locale

instance ToJSONKey Locale where
  toJSONKey = ToJSONKeyValue (\(Locale s) -> String s) toEncoding

instance FromJSONKey Locale

deriving instance Generic Msgid

instance ToJSONKey Msgid where
  toJSONKey = ToJSONKeyValue (\(Msgid s) -> String s) toEncoding

instance FromJSONKey Msgid

instance ToJSONKey (Maybe I18n.Context) where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSONKey (Maybe I18n.Context)


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
makeRefineTypes [''Locale, ''Msgid, ''L10, ''GetTranslations, ''Trans]  -- FIXME: Trans should be an abstract type.  we need to be more restrictive than that.
