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

module Refine.Frontend.ThirdPartyViews
  ( sticky_
  , stickyContainer_
  , skylight_
  , hammer_
  , editor_
  ) where

import GHCJS.Types
import React.Flux


-- TODO: test that this doesn't crash because the foreign object is not loaded.  this has bitten us
-- at least twice in the past.


sticky_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
sticky_ = foreignClass js_sticky

foreign import javascript unsafe "Sticky.Sticky" js_sticky :: JSVal


stickyContainer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
stickyContainer_ = foreignClass js_stickyContainer

foreign import javascript unsafe "Sticky.StickyContainer" js_stickyContainer :: JSVal


skylight_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
skylight_ = foreignClass js_skylight

foreign import javascript unsafe "Skylight.SkyLightStateless" js_skylight :: JSVal


hammer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
hammer_ = foreignClass js_hammer

foreign import javascript unsafe "Hammer" js_hammer :: JSVal


editor_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editor_ = foreignClass js_editor

foreign import javascript unsafe "Draft.Editor" js_editor :: JSVal
