{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Translation where
#include "import.hs"

import Data.Map (filterWithKey)
import Data.Text.I18n.Po

import Refine.Backend.App.Core
import Refine.Backend.Config
import Refine.Common.Types.Translation


-- | Return the translation table for one 'Locale' (loading all translation tables no matter what the
-- locale settings would use too much bandwidth).
--
-- FIXME: this always loads all translation tables from disk before selecting one of them to send it
-- to the browser.  We should load all of them either at start time or at build time (see FUTUREWORK
-- below).
--
-- FUTUREWORK: Bake the PO files into the server binary using 'readFile' from within
-- TemplateHaskell.  This is easier on the server load, but more importantly eliminates the
-- 'AppL10ParseErrors' constructor because those are all compile-time errors.
getTranslations :: GetTranslations -> AppIO L10
getTranslations (GetTranslations locale) = do
  appLog "getTranslations"
  poFileRoot <- asks . view $ appConfig . cfgPoFilesRoot
  join . liftIO $ do
    (l10, parseErrors) <- getL10n poFileRoot
    pure $ do
      unless (null parseErrors) $ do
        throwError . AppL10ParseErrors $ map (cs . show) parseErrors
      pure $ L10 (Map.filterWithKey (\k _ -> k == locale) l10) locale
