{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Screen.Types where
#include "import_frontend.hs"


data ScreenAction =
    AddHeaderHeight Int
  | SetWindowWidth Int
  deriving (Show, Eq, Generic)

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Eq, Generic)

data ScreenState = ScreenState
  { _ssHeaderHeight           :: Int  -- ^ without toolbar (FIXME: this only makes sense when in a vdoc.  move it there?)
  , _ssWindowWidth            :: Int
  , _ssWindowSize             :: WindowSize
  } deriving (Show, Eq, Generic)

emptyScreenState :: HasCallStack => ScreenState
emptyScreenState = ScreenState 0 0 Desktop

-- | (should we measure this?)
fixedHeaderHeight :: Int
fixedHeaderHeight = 80


makeRefineTypes [''ScreenState, ''ScreenAction, ''WindowSize]
