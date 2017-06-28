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

module Refine.Frontend.Screen.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)


data ScreenAction =
    AddHeaderHeight Int
  | SetWindowWidth Int
  deriving (Show, Eq, Generic)

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Eq, Generic)

data ScreenState = ScreenState
  { _ssHeaderHeight           :: Int  -- ^ without toolbar
  , _ssWindowWidth            :: Int
  , _ssWindowSize             :: WindowSize
  } deriving (Show, Eq, Generic)

emptyScreenState :: HasCallStack => ScreenState
emptyScreenState = ScreenState 0 0 Desktop

-- | (should we measure this?)
fixedHeaderHeight :: Int
fixedHeaderHeight = 80


makeRefineTypes [''ScreenState, ''ScreenAction, ''WindowSize]
