{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Refine.Frontend.Screen.Types where

import GHC.Generics (Generic)

import Refine.Prelude.TH (makeRefineType)


type OffsetFromViewportTop = Int
type ScrollOffsetOfViewport = Int

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Generic)


data ScreenState = ScreenState
  { _ssHeaderHeight           :: Int
  , _ssWindowSize             :: WindowSize
  } deriving (Show, Generic)

emptyScreenState :: ScreenState
emptyScreenState = ScreenState 0 Desktop


makeRefineType ''ScreenState
makeRefineType ''WindowSize
