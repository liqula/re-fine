{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Mark where

import           Control.Concurrent (forkIO)
import           Control.Lens ((^.))
import           Data.Int
import           Data.List (find)
import           Data.Maybe (isJust)
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.String.Conversions
import           GHCJS.Types (JSString, JSVal)
import           React.Flux
import           React.Flux.Lifecycle
import qualified Text.HTML.Parser as HTMLP
import           Text.Read (readMaybe)

import           Refine.Common.Types
import           Refine.Common.Rest

import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


data MarkProps = MarkProps
  { _dataHunkId :: Int64
  , _dataContentType :: String
  }

toMarkProps :: [HTMLP.Attr] -> CompositeVDoc -> MarkProps
toMarkProps attrs vdoc = let maybeChunkId = Text.Read.readMaybe . cs $ chunkIdIn attrs :: Maybe Int64
  in case maybeChunkId of
    Nothing -> MarkProps (-1) ""
    Just chunkId -> MarkProps chunkId (kindOf chunkId)
  where
    chunkIdIn [] = ""
    chunkIdIn ((HTMLP.Attr "data-chunk-id" value):_) = value
    chunkIdIn (_:as) = chunkIdIn as
    -- TODO better annotate the mark tokens with the kind ?!
    kindOf chunkId =
      let isNote = find (\(Note (ID noteId) _ _ _) -> noteId == chunkId) (vdoc ^. compositeVDocNotes)
          isDiscussion = find (\(Comment (ID discId) _ _ _) -> discId == chunkId) (vdoc ^. compositeVDocComments)
      in if isJust isNote then "note" else if isJust isDiscussion then "discussion" else ""


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
                 let actions = RS.dispatch $ RS.AddMarkPosition (_dataHunkId props) top
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
  js_getBoundingClientRectTop :: JSVal -> IO Int

foreign import javascript unsafe
  "console.log($1)"
  consoleLog :: JSString -> IO ()
