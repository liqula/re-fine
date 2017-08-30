{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
#include "language_frontend.hs"
module Refine.Frontend.Prelude
  ( module P
  , module Refine.Frontend.Prelude
  ) where

import GHCJS.Foreign.Callback as P (Callback, asyncCallback)
import GHCJS.Marshal as P
import GHCJS.Marshal.Pure as P
import GHCJS.Types as P
import React.Flux as P hiding (on, embed_, style)

import Refine.Common.Prelude as P
import Refine.Common.Types.Prelude as P (ID, User, miID)
import Refine.Frontend.CS as P

import qualified React.Flux.Outdated as React
import           Control.Concurrent
import           Debug.Trace

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

-- helps to workaround #401
type family View' (xs :: [*]) where
  View' '[x] = React.ReactView x

-- helps to workaround #401
view_' :: forall props eventHandler . Typeable props
       => View' '[props] -> JSString -> props -> ReactElementM eventHandler ()
view_' w key ps = React.viewWithSKey @props w key ps mempty

-- helps to workaround #401
mkView'
    :: (Eq props, Typeable props)
    => JSString
    -> (props
       -> ReactElementM_ (StatefulViewEventHandler ()) ())
    -> View' '[props]
mkView' name f =
  React.defineLifecycleView (cs name) () React.lifecycleConfig { React.lRender = \() -> f }

-- helper to debug issues like #401
testReRender_ :: Int -> ReactElementM eventHandler ()
testReRender_ = view_ testReRender "testReRender_"

testReRender :: View '[Int]
testReRender = mkView "testReRender" $ \i ->
  trace ("testReRender elem #" <> show i <> "is drawn") . elemText . cs $ show i
