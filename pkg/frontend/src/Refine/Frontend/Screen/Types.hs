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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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
