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

import           Data.String.Conversions
import           Data.Text (split)
import           React.Flux

import           Refine.Common.Types
import           Refine.Frontend.CS ()


data DocumentHeaderProps = DocumentHeaderProps
  { _headerTitle :: Title
  , _headerAbstract :: Abstract
  }
  deriving (Eq)

instance UnoverlapAllEq DocumentHeaderProps

documentHeader :: View '[DocumentHeaderProps]
documentHeader = mkView "DocumentHeader" $ \props ->
  div_ ["className" $= "row row-align-middle c-vdoc-header"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
          div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
              documentTitle_ $ _headerTitle props
              documentAbstract_ $ _headerAbstract props
              phases_

documentHeader_ :: DocumentHeaderProps -> ReactElementM eventHandler ()
documentHeader_ !props = view_ documentHeader "DocumentHeader_" props

instance UnoverlapAllEq Title

documentTitle :: View '[Title]
documentTitle = mkView "DocumentTitle" $ h1_ . cs . _unTitle

documentTitle_ :: Title -> ReactElementM eventHandler ()
documentTitle_ !title = view_ documentTitle "DocumentTitle_" title

instance UnoverlapAllEq Abstract

documentAbstract :: View '[Abstract]
documentAbstract = mkView "DocumentAbstract" $ \abstract ->
  div_ ["className" $= "c-vdoc-header__description"] $ do
    let paragraphs = split (== '\n') . cs $ _unAbstract abstract
    div_ ["className" $= "c-vdoc-header__description"] . mconcat $ (p_ . elemText) <$> paragraphs

documentAbstract_ :: Abstract -> ReactElementM eventHandler ()
documentAbstract_ !abstract = view_ documentAbstract "DocumentAbstract_" abstract


phases :: View '[]
phases = mkView "Phases" $ do
  div_ ["className" $= "c-vdoc-header__phases"] $ do
    h5_ "Phases"
    div_ ["className" $= "c-vdoc-header__phase c-vdoc-header__phase--active"] "Text Collaboration"
    div_ ["className" $= "c-vdoc-header__phase"] "Vote"
    div_ ["className" $= "c-vdoc-header__phase"] "Result"


phases_ :: ReactElementM eventHandler ()
phases_ = view_ phases "Phases_"
