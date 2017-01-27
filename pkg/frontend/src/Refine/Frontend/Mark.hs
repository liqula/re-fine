{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Mark where

import           Control.Concurrent (forkIO)
import           Data.Int
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.String.Conversions
import           GHCJS.Types (JSString, JSVal)
import           React.Flux
import           React.Flux.Lifecycle
import qualified Text.HTML.Parser as HTMLP
import           Text.Read (readMaybe)

import qualified Refine.Frontend.Screen.Types as RS
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


data MarkProps = MarkProps
  { _dataHunkId :: Int64
  , _dataContentType :: String
  }

toMarkProps :: [HTMLP.Attr] -> MarkProps
toMarkProps attrs = let maybeChunkId = readMaybe $ valueOf "data-chunk-id" attrs :: Maybe Int64
  in case maybeChunkId of
    Nothing -> MarkProps (-1) ""
    Just chunkId -> MarkProps chunkId (valueOf "data-chunk-kind" attrs)
  where
    valueOf :: String -> [HTMLP.Attr] -> String
    valueOf _ [] = ""
    valueOf wantedKey (HTMLP.Attr key value:_) | key == cs wantedKey = cs value
    valueOf wantedKey (_:as) = valueOf wantedKey as


rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
   { lRender = \_state props ->
         mark_ [ "data-hunk-id" $= fromString (show (_dataHunkId props))
               , "className" $= fromString ("o-mark o-mark--" <> _dataContentType props)
               ] childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             consoleLog "Component did mount"
             this <- lThis ldom
             top <- js_getBoundingClientRectTop this
             props <- lGetProps propsandstate
             _ <- forkIO $ do
                 let actions = RS.dispatch $ RS.AddMarkPosition (_dataHunkId props) top 0 -- we assume that no scrolling has taken place yet
                 forM_ actions executeAction
             return ()

   , lComponentDidUpdate = Just $ \_ ldom _ _ _ -> do
             consoleLog "Component did update"
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
  js_getBoundingClientRectTop :: JSVal -> IO RS.OffsetFromViewportTop

foreign import javascript unsafe
  "console.log($1)"
  consoleLog :: JSString -> IO ()
