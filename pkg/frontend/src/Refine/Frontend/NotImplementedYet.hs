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

module Refine.Frontend.NotImplementedYet where

import React.Flux

import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Frontend.Style
import           Refine.Frontend.ThirdPartyViews (skylight_)

dialogStyles :: [Style]
dialogStyles =
  [ StyleEm "width" 23
  , StyleEm "height" 8
  -- , StyleEm "left" 7.5
  -- , StyleCword "top" "undefined"
  , StyleEm "marginLeft" 0
  , StyleEm "marginTop" (-11.5)
  , StyleST "textAlign" "left"
  , StyleInt "zIndex" 6050
  ]

overlayStyles :: [Style]
overlayStyles =
  [ StyleInt "zIndex" 6010
  ]

instance UnoverlapAllEq Bool

notImplementedYet :: View '[Bool]
notImplementedYet = mkView "NotImplementedYet" $ \isVisible ->
  div_ [onClick $ \e _ -> stopPropagation e `seq` RS.dispatch RS.HideNotImplementedYet] $ do
    skylight_ ["isVisible" &= isVisible
             , "dialogStyles" @= dialogStyles
             , "overlayStyles" @= overlayStyles
             , on "onOverlayClicked" $ \_ -> RS.dispatch RS.HideNotImplementedYet
             , on "onCloseClicked" $ \_ -> RS.dispatch RS.HideNotImplementedYet
             ] $ do
      div_ "This feature is not implemented yet."


notImplementedYet_ :: Bool -> ReactElementM eventHandler ()
notImplementedYet_ !isVisible = view_ notImplementedYet "notImplementedYet_" isVisible
