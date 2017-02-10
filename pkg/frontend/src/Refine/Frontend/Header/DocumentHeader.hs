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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.DocumentHeader where

import           Data.String.Conversions
import           Data.Text (split)
import           React.Flux

import           Refine.Common.Types


data DocumentHeaderProps = DocumentHeaderProps
  { _headerTitle :: Title
  , _headerAbstract :: Abstract
  }

documentHeader :: ReactView DocumentHeaderProps
documentHeader = defineView "DocumentHeader" $ \props ->
  div_ ["className" $= "row row-align-middle c-vdoc-header"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
          div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
              documentTitle_ $ _headerTitle props
              documentAbstract_ $ _headerAbstract props
              phases_

documentHeader_ :: DocumentHeaderProps -> ReactElementM eventHandler ()
documentHeader_ props = view documentHeader props mempty

documentTitle :: ReactView Title
documentTitle = defineView "DocumentTitle" $ \title ->
  h1_ . elemText . cs $ _unTitle title

documentTitle_ :: Title -> ReactElementM eventHandler ()
documentTitle_ title = view documentTitle title mempty

documentAbstract :: ReactView Abstract
documentAbstract = defineView "DocumentAbstract" $ \abstract ->
  div_ ["className" $= "c-vdoc-header__description"] $ do
    let paragraphs = split (== '\n') . cs $ _unAbstract abstract
    div_ ["className" $= "c-vdoc-header__description"] . mconcat $ (p_ . elemText) <$> paragraphs

documentAbstract_ :: Abstract -> ReactElementM eventHandler ()
documentAbstract_ abstract = view documentAbstract abstract mempty


phases :: ReactView ()
phases = defineView "Phases" $ \() ->
  div_ ["className" $= "c-vdoc-header__phases"] $ do
    h5_ "Phases"
    div_ ["className" $= "c-vdoc-header__phase c-vdoc-header__phase--active"] "Text Collaboration"
    div_ ["className" $= "c-vdoc-header__phase"] "Vote"
    div_ ["className" $= "c-vdoc-header__phase"] "Result"


phases_ :: ReactElementM eventHandler ()
phases_ = view phases () mempty
