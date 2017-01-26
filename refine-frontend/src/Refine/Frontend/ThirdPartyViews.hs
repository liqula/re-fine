{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.ThirdPartyViews
  ( hammer_
  , overlay_
  , sticky_
  , stickyContainer_
  ) where

import           React.Flux


sticky_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
sticky_ = foreign_ "Sticky"

stickyContainer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
stickyContainer_ = foreign_ "StickyContainer"

overlay_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
overlay_ = foreign_ "Overlay" -- SkyLightStateless from react-skylight

hammer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
hammer_ = foreign_ "Hammer"
