{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Mark where

import           Control.Concurrent (forkIO)
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Lifecycle

import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


data MarkProps = MarkProps
  { _dataHunkId :: String
  , _dataContentType :: String
  }

rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
   { lRender = \_state props ->
         mark_ [ "data-hunk-id" $= fromString (_dataHunkId props)
               , "className" $= fromString ("o-mark o-mark--" <> _dataContentType props)
               ] childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             this <- lThis ldom
             top <- js_getBoundingClientRectTop this
             props <- lGetProps propsandstate
             _ <- forkIO $ do
                 let actions = RS.dispatch $ RS.AddMarkPosition (_dataHunkId props) top
                 forM_ actions executeAction
             return ()

   , lComponentDidUpdate = Just $ \_ ldom _ _ _ -> do
             this <- lThis ldom
             -- _ <- consoleLog this
             _top <- js_getBoundingClientRectTop this
             return ()
             -- consoleLog $ show top
   }

rfMark_ :: MarkProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
rfMark_ = view rfMark

foreign import javascript unsafe
  "$1.getBoundingClientRect().top"
  js_getBoundingClientRectTop :: JSVal -> IO Int
