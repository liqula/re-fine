{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Screen.Store
  ( screenStateUpdate
  ) where
#include "import_frontend.hs"

import Refine.Frontend.Screen.Types

screenStateUpdate :: HasCallStack => ScreenAction -> ScreenState -> ScreenState
screenStateUpdate act st = st
  & ssHeaderHeight         %~ headerHeightUpdate act
  & ssWindowWidth          %~ windowWidthUpdate act
  & ssWindowSize           %~ windowSizeUpdate act


headerHeightUpdate :: HasCallStack => ScreenAction -> Int -> Int
headerHeightUpdate act st = case act of
    AddHeaderHeight height -> height
    _ -> st


windowWidthUpdate :: HasCallStack => ScreenAction -> Int -> Int
windowWidthUpdate act st = case act of
    SetWindowWidth width -> width
    _ -> st


windowSizeUpdate :: HasCallStack => ScreenAction -> WindowSize -> WindowSize
windowSizeUpdate act st = case act of
  SetWindowWidth width -> toSize width
  _ -> st


toSize :: HasCallStack => Int -> WindowSize
toSize sz
  | sz <= 480  = Mobile
  | sz <= 1024 = Tablet
  | otherwise  = Desktop
