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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Header.DocumentHeader where

import Refine.Frontend.Prelude

import           Control.Lens (ix)
import qualified Data.Text as ST

import           Refine.Common.Types


data DocumentHeaderProps = DocumentHeaderProps
  { _headerTitle :: Title
  , _headerAbstract :: Abstract
  }
  deriving (Eq)

instance UnoverlapAllEq DocumentHeaderProps

documentHeader :: HasCallStack => View '[DocumentHeaderProps]
documentHeader = mkView "DocumentHeader" $ \props ->
  div_ ["className" $= "row row-align-middle c-vdoc-header"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
          div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
              documentTitle_ $ _headerTitle props
              documentAbstract_ $ _headerAbstract props
              phases_

documentHeader_ :: HasCallStack => DocumentHeaderProps -> ReactElementM eventHandler ()
documentHeader_ !props = view_ documentHeader "DocumentHeader_" props

instance UnoverlapAllEq Title


documentTitle :: HasCallStack => View '[Title]
documentTitle = mkView "DocumentTitle" $ h1_ . cs . _unTitle

documentTitle_ :: HasCallStack => Title -> ReactElementM eventHandler ()
documentTitle_ !title = view_ documentTitle "DocumentTitle_" title

instance UnoverlapAllEq Abstract


documentAbstract :: HasCallStack => View '[Abstract]
documentAbstract = mkView "DocumentAbstract" $ \abstract ->
  div_ ["className" $= "c-vdoc-header__description"] $ do
    let paragraphs = ST.split (== '\n') . cs $ _unAbstract abstract
    div_ ["className" $= "c-vdoc-header__description"] . mconcat $ (p_ . elemText) <$> paragraphs

documentAbstract_ :: HasCallStack => Abstract -> ReactElementM eventHandler ()
documentAbstract_ !abstract = view_ documentAbstract "DocumentAbstract_" abstract


phases :: HasCallStack => View '[]
phases = mkView "Phases" $ do
  div_ ["className" $= "c-vdoc-header__phases"] $ do
    h5_ "Phases"
    div_ ["className" $= "c-vdoc-header__phase c-vdoc-header__phase--active"] "Text Collaboration"
    div_ ["className" $= "c-vdoc-header__phase"] "Vote"
    div_ ["className" $= "c-vdoc-header__phase"] "Result"

phases_ :: HasCallStack => ReactElementM eventHandler ()
phases_ = view_ phases "Phases_"


editDescToAbstract :: HasCallStack => CompositeVDoc -> ContributionID -> Abstract
editDescToAbstract vdoc (ContribIDEdit eid) = Abstract $
  "Edit Request:\n" <>
  (vdoc ^?! compositeVDocApplicableEdits . ix eid . editDesc)
editDescToAbstract _ _ = error "internal error."
