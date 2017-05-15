{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Translation where

import Control.Lens (view)
import Control.Monad (join, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Map as Map (filterWithKey)
import Data.String.Conversions (cs)
import Data.Text.I18n.Po

import Refine.Backend.App.Core
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
getTranslations :: GetTranslations -> App L10
getTranslations (GetTranslations locale) = do
  appLog "getTranslations"
  poFileRoot <- asks (view appPoFilesRoot)
  join . appIO $ do
    (l10, parseErrors) <- getL10n poFileRoot
    pure $ do
      unless (null parseErrors) $ do
        throwError . AppL10ParseErrors $ map (cs . show) parseErrors
      pure $ L10 (Map.filterWithKey (\k _ -> k == locale) l10) locale
