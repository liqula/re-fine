{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Refine.Frontend.Screen.Types where

import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)


type OffsetFromViewportTop = Int
type ScrollOffsetOfViewport = Int

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Typeable, Generic, NFData, ToJSON)


data ScreenState = ScreenState
  { _ssHeaderHeight           :: Int
  , _ssWindowSize             :: WindowSize
  } deriving (Show, Typeable, Generic, NFData, ToJSON)

makeLenses ''ScreenState

emptyScreenState :: ScreenState
emptyScreenState = ScreenState 0 Desktop
