{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
module Refine.Frontend.Prelude
  ( module P
  , module Refine.Frontend.Prelude
  ) where

import Data.JSString as P ()  -- @instance IsString JSString@, mostly
import GHCJS.Foreign.Callback as P (Callback, asyncCallback)
import GHCJS.Marshal as P
import GHCJS.Marshal.Pure as P
import GHCJS.Types as P
import React.Flux as P hiding (on, embed_, style)

import Refine.Common.Prelude as P
import Refine.Common.Types.Prelude as P (ID, User, miID)
import Refine.Frontend.CS as P

import Control.Concurrent

class Dispatchable action where
  dispatch :: action -> ViewEventHandler

dispatchM :: (Dispatchable action, HasCallStack) => Monad m => action -> m ViewEventHandler
dispatchM = pure . dispatch

reDispatchM :: (Dispatchable action, HasCallStack) => MonadState [action] m => action -> m ()
reDispatchM a = modify (<> [a])

dispatchAndExec :: (Dispatchable action, HasCallStack) => MonadIO m => action -> m ()
dispatchAndExec a = liftIO . void . forkIO $ do
  () <- executeAction `mapM_` dispatch a
  pure ()

-- to break a module dependency cycle
class Sendable message where
  sendToServer :: message -> IO ()
