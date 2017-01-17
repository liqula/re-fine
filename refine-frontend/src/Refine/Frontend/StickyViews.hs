{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.StickyViews (
    sticky_
  , stickyContainer_
  ) where

import           React.Flux


sticky_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
sticky_ = foreign_ "Sticky"

stickyContainer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
stickyContainer_ = foreign_ "StickyContainer"

