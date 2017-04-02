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


makeRefineType ''ScreenState
makeRefineType ''ScreenAction
makeRefineType ''WindowSize
