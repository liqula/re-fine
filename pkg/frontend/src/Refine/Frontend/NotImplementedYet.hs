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
import           Refine.Frontend.Style
import           Refine.Frontend.ThirdPartyViews (skylight_)
import qualified Refine.Frontend.Types as RS

dialogStyles :: [Style]
dialogStyles =
  [ Style "width" ("23em" :: String)
  , Style "height" ("8em" :: String)
  -- , Style "left" ("7.5em" :: String)
  -- , Style "top" ("undefined" :: String)
  , Style "marginLeft" ("0" :: String)
  , Style "marginTop" ("-11.5em" :: String)
  , Style "textAlign" ("left" :: String)
  , Style "zIndex" (6050 :: Int)
  ]

overlayStyles :: [Style]
overlayStyles =
  [ Style "zIndex" (6010 :: Int)
  ]

notImplementedYet :: ReactView Bool
notImplementedYet = defineView "NotImplementedYet" $ \isVisible ->
  skylight_ ["isVisible" &= isVisible
           , "dialogStyles" @= dialogStyles
           , "overlayStyles" @= overlayStyles
           , on "onOverlayClicked" $ \_ -> RS.dispatch RS.HideNotImplementedYet
           , on "onCloseClicked" $ \_ -> RS.dispatch RS.HideNotImplementedYet
           ] $ do
    div_ "This feature is not implemented yet."


notImplementedYet_ :: Bool -> ReactElementM eventHandler ()
notImplementedYet_ isVisible = view notImplementedYet isVisible mempty
