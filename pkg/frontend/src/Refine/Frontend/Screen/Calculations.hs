module Refine.Frontend.Screen.Calculations where

import           Control.Lens ((^.))

import           Refine.Frontend.Screen.Types


offsetIntoText :: Int -> Int -> ScreenState -> Int
offsetIntoText offsetFromScreenTop scrollOffset state = offsetFromScreenTop + scrollOffset - state ^. ssHeaderHeight - 80

