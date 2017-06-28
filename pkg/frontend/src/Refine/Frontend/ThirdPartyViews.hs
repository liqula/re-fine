{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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
  ( ThirdPartyView
  , sticky_
  , stickyContainer_
  , skylight_
  , hammer_
  , editor_
  ) where

import Refine.Frontend.Prelude


type ThirdPartyView eventHandler = [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()

sticky_ :: HasCallStack => ThirdPartyView eventHandler
sticky_ = foreignClass js_sticky

stickyContainer_ :: HasCallStack => ThirdPartyView eventHandler
stickyContainer_ = foreignClass js_stickyContainer

skylight_ :: HasCallStack => ThirdPartyView eventHandler
skylight_ = foreignClass js_skylight

hammer_ :: HasCallStack => ThirdPartyView eventHandler
hammer_ = foreignClass js_hammer

editor_ :: HasCallStack => ThirdPartyView eventHandler
editor_ = foreignClass js_editor

#ifdef __GHCJS__

foreign import javascript safe
  "Sticky.Sticky"
  js_sticky :: JSVal

foreign import javascript safe
  "Sticky.StickyContainer"
  js_stickyContainer :: JSVal

foreign import javascript safe
  "Skylight.SkyLightStateless"
  js_skylight :: JSVal

foreign import javascript safe
  "Hammer"
  js_hammer :: JSVal

foreign import javascript safe
  "Draft.Editor"
  js_editor :: JSVal

#else

{-# ANN js_sticky ("HLint: ignore Use camelCase" :: String) #-}
js_sticky :: JSVal
js_sticky = error "javascript FFI not available in GHC"

{-# ANN js_stickyContainer ("HLint: ignore Use camelCase" :: String) #-}
js_stickyContainer :: JSVal
js_stickyContainer = error "javascript FFI not available in GHC"

{-# ANN js_skylight ("HLint: ignore Use camelCase" :: String) #-}
js_skylight :: JSVal
js_skylight = error "javascript FFI not available in GHC"

{-# ANN js_hammer ("HLint: ignore Use camelCase" :: String) #-}
js_hammer :: JSVal
js_hammer = error "javascript FFI not available in GHC"

{-# ANN js_editor ("HLint: ignore Use camelCase" :: String) #-}
js_editor :: JSVal
js_editor = error "javascript FFI not available in GHC"

#endif
