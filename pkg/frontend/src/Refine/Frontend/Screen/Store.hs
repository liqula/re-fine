{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Refine.Frontend.Screen.Store where

import           Control.Lens ((&), (%~))

import Refine.Frontend.Screen.Types
import Refine.Frontend.Types

screenStateUpdate :: RefineAction -> ScreenState -> ScreenState
screenStateUpdate action state =
  let newState = state
                  & ssHeaderHeight         %~ headerHeightUpdate action
                  & ssWindowSize           %~ windowSizeUpdate action
  in newState

---------------------------------------------------------------------------

headerHeightUpdate :: RefineAction -> Int -> Int
headerHeightUpdate action state = case action of
    AddHeaderHeight height -> height
    _ -> state

windowSizeUpdate :: RefineAction -> WindowSize -> WindowSize
windowSizeUpdate action state = case action of
    SetWindowSize newSize -> newSize
    _ -> state

