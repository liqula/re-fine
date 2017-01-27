module Refine.Frontend.Screen.Calculations where

import           Control.Lens ((^.))

import           Refine.Frontend.Screen.Types

offsetIntoText :: OffsetFromViewportTop -> ScrollOffsetOfViewport -> ScreenState -> Int
offsetIntoText offsetFromScreenTop scrollOffset state = offsetFromScreenTop + scrollOffset - state ^. ssHeaderHeight - 80

