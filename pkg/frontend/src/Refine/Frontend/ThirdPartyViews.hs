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
  ( hammer_
  , skylight_
  , sticky_
  , stickyContainer_
  ) where

import           React.Flux


sticky_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
sticky_ = foreign_ "Sticky"

stickyContainer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
stickyContainer_ = foreign_ "StickyContainer"

skylight_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
skylight_ = foreign_ "Skylight" -- SkyLightStateless from react-skylight

hammer_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
hammer_ = foreign_ "Hammer"
