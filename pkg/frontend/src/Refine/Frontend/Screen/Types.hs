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

module Refine.Frontend.Screen.Types where

import GHC.Generics (Generic)

import Refine.Prelude.TH (makeRefineType)


-- | `viewport` is the browser window (the visible part of the `document`, or `page`).  See
-- `/docs/frontend/offsets.pdf`.
newtype OffsetFromViewportTop = OffsetFromViewportTop { _unOffsetFromViewportTop :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

-- | Distance between document top and viewport top.  See `/docs/frontend/offsets.pdf`.
newtype ScrollOffsetOfViewport = ScrollOffsetOfViewport { _unScrollOffsetOfViewport :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

-- | Distance between document top and node (e.g., `<mark>` or `</mark>`).
newtype OffsetFromDocumentTop = OffsetFromDocumentTop { _unOffsetFromDocumentTop :: Int }
  deriving (Show, Generic, Eq, Ord, Num)


data ScreenAction =
    AddHeaderHeight Int
  | SetWindowWidth Int
  deriving (Show, Eq, Generic)

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Eq, Generic)

data ScreenState = ScreenState
  { _ssHeaderHeight           :: Int
  , _ssWindowWidth            :: Int
  , _ssWindowSize             :: WindowSize
  } deriving (Show, Eq, Generic)

emptyScreenState :: ScreenState
emptyScreenState = ScreenState 0 0 Desktop


makeRefineType ''OffsetFromViewportTop
makeRefineType ''ScrollOffsetOfViewport
makeRefineType ''OffsetFromDocumentTop
makeRefineType ''ScreenState
makeRefineType ''ScreenAction
makeRefineType ''WindowSize
