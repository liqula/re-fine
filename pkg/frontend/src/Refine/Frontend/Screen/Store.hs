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


module Refine.Frontend.Screen.Store
( screenStateUpdate
) where

import           Control.Lens ((&), (%~))

import Refine.Frontend.Screen.Types

screenStateUpdate :: ScreenAction -> ScreenState -> ScreenState
screenStateUpdate action state =
  let newState = state
                  & ssHeaderHeight         %~ headerHeightUpdate action
                  & ssWindowWidth          %~ windowWidthUpdate action
                  & ssWindowSize           %~ windowSizeUpdate action
  in newState

---------------------------------------------------------------------------

headerHeightUpdate :: ScreenAction -> Int -> Int
headerHeightUpdate action state = case action of
    AddHeaderHeight height -> height
    _ -> state


windowWidthUpdate :: ScreenAction -> Int -> Int
windowWidthUpdate action state = case action of
    SetWindowWidth width -> width
    _ -> state


windowSizeUpdate :: ScreenAction -> WindowSize -> WindowSize
windowSizeUpdate action state = case action of
  SetWindowWidth width -> toSize width
  _ -> state


toSize :: Int -> WindowSize
toSize sz
  | sz <= 480  = Mobile
  | sz <= 1024 = Tablet
  | otherwise  = Desktop



