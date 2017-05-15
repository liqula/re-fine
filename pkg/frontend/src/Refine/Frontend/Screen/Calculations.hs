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

module Refine.Frontend.Screen.Calculations where

import Refine.Frontend.Prelude

import           Control.Lens ((^.))

import           Refine.Frontend.Types
import           Refine.Frontend.Screen.Types


offsetIntoText :: OffsetFromDocumentTop -> ScreenState -> Int
offsetIntoText (OffsetFromDocumentTop topOffset) state = topOffset - state ^. ssHeaderHeight - 80

offsetFromDocumentTop :: OffsetFromViewportTop -> ScrollOffsetOfViewport -> OffsetFromDocumentTop
offsetFromDocumentTop (OffsetFromViewportTop v) (ScrollOffsetOfViewport d) = OffsetFromDocumentTop (v + d)
