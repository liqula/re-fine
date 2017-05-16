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


module Refine.Frontend.Screen.Store
  ( screenStateUpdate
  ) where

import Refine.Frontend.Prelude

import           Control.Lens ((&), (%~))

import Refine.Frontend.Screen.Types

screenStateUpdate :: ScreenAction -> ScreenState -> ScreenState
screenStateUpdate action st = st
  & ssHeaderHeight         %~ headerHeightUpdate action
  & ssWindowWidth          %~ windowWidthUpdate action
  & ssWindowSize           %~ windowSizeUpdate action


headerHeightUpdate :: ScreenAction -> Int -> Int
headerHeightUpdate action st = case action of
    AddHeaderHeight height -> height
    _ -> st


windowWidthUpdate :: ScreenAction -> Int -> Int
windowWidthUpdate action st = case action of
    SetWindowWidth width -> width
    _ -> st


windowSizeUpdate :: ScreenAction -> WindowSize -> WindowSize
windowSizeUpdate action st = case action of
  SetWindowWidth width -> toSize width
  _ -> st


toSize :: Int -> WindowSize
toSize sz
  | sz <= 480  = Mobile
  | sz <= 1024 = Tablet
  | otherwise  = Desktop
