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

import           Refine.Frontend.Types
import           Refine.Frontend.Screen.Types


-- | This is the *actual* offset from page top (i think), and should perhaps be typed
-- 'OffsetFromHtmlTop'; and 'OffsetFromDocumentTop' then probably means 'OffsetFromArticleTop'.
-- None of this code is terribly easy to understand...  :(
offsetIntoText :: HasCallStack => OffsetFromDocumentTop -> ScreenState -> Int
offsetIntoText (OffsetFromDocumentTop topOffset) st = topOffset - st ^. ssHeaderHeight - 80

offsetFromDocumentTop :: HasCallStack => OffsetFromViewportTop -> ScrollOffsetOfViewport -> OffsetFromDocumentTop
offsetFromDocumentTop (OffsetFromViewportTop v) (ScrollOffsetOfViewport d) = OffsetFromDocumentTop (v + d)
