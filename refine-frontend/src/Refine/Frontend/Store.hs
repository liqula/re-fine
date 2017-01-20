{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Store where

import           Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Aeson as AE
import qualified Data.Map.Strict as M
import           React.Flux
import           Data.Aeson (ToJSON, encode)
import           Data.String.Conversions
import           Data.JSString (JSString, pack, unpack)

import Refine.Common.Rest
import Refine.Common.Types
import Refine.Frontend.Rest
import Refine.Frontend.Test.Samples
import Refine.Frontend.Types


toSize :: Int -> WindowSize
toSize sz
  | sz <= 480  = Mobile
  | sz <= 1024 = Tablet
  | otherwise  = Desktop

instance StoreData GlobalState where
    type StoreAction GlobalState = RefineAction
    transform action state = do
        consoleLog "Old state: " state
        consoleLog "Action: " action

        emitBackendCallsFor action state

        -- TODO can this be improved?
        selectedRangeOrState <- case action of -- for efficiency reasons, don't ask JS for each action
            SetSelection deviceOffset -> do
                hasRange <- js_hasRange
                range <- if hasRange then getRange else return Nothing
                return (range, Just deviceOffset)
            _ -> return $ _gsCurrentSelection state


        let newState = state
              & gsVDoc             %~ vdocUpdate action
              & gsVDocList         %~ vdocListUpdate action
              & gsHeaderHeight     %~ headerHeightUpdate action
              & gsMarkPositions    %~ markPositionsUpdate action
              & gsWindowSize       %~ windowSizeUpdate action
              & gsCurrentSelection .~ currentSelectionUpdate action selectedRangeOrState    -- TODO can this be improved?

        consoleLog "New state: " newState
        return newState


vdocUpdate :: RefineAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action state = case action of
    OpenDocument openedVDoc -> Just openedVDoc
    _ -> state

vdocListUpdate :: RefineAction -> Maybe [ID VDoc] -> Maybe [ID VDoc]
vdocListUpdate action state = case action of
    LoadedDocumentList list -> Just list
    _ -> state

headerHeightUpdate :: RefineAction -> Int -> Int
headerHeightUpdate action state = case action of
    AddHeaderHeight height -> height
    _ -> state

markPositionsUpdate :: RefineAction -> MarkPositions -> MarkPositions
markPositionsUpdate action state = case action of
    AddMarkPosition dataHunkId pos -> MarkPositions $ M.alter (\_ -> Just pos) dataHunkId (_unMarkPositions state)
    _ -> state

windowSizeUpdate :: RefineAction -> WindowSize -> WindowSize
windowSizeUpdate action state = case action of
    SetWindowSize newSize -> newSize
    _ -> state

currentSelectionUpdate :: RefineAction -> (Maybe Range, Maybe DeviceOffset) -> (Maybe Range, Maybe DeviceOffset)
currentSelectionUpdate action state = case action of
    SetSelection _ -> state -- TODO this only works because of how this is invoked -- needs improvement!
    SubmitPatch -> (Nothing, Nothing)
    _ -> state



emitBackendCallsFor :: RefineAction -> GlobalState -> IO ()
emitBackendCallsFor action _state = case action of
    LoadDocumentList -> do
        listVDocs $ \case
            (Left(_, msg)) -> handleError msg
            (Right loadedVDocs) -> return . dispatch $ LoadedDocumentList ((^. vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left(_, msg)) -> handleError msg
            (Right loadedVDoc) -> return . dispatch $ OpenDocument loadedVDoc

    AddDemoDocument -> do
        createVDoc (CreateVDoc sampleTitle sampleAbstract sampleText) $ \case
            (Left(_, msg)) -> handleError msg
            (Right loadedVDoc) -> return . dispatch $ OpenDocument loadedVDoc

{- TODO submitting a patch does not work yet
    SubmitPatch -> do
        let vdocId = _metaKey . _vdocMeta . fromJust $ _vdoc state
        let patchId = _vdocHead . fromJust $ _vdoc state
        let vdocChunk = VDocChunk "<p><strong>This is my new and obviously much, much better text :-)</strong></p>"
        let maybeRange = _currentSelection state
        let patchKey = PatchKey vdocId patchId
        case maybeRange of
            (Nothing, _)    -> return ()
            (Just range, _) -> do
                                let protoChunkRange = ProtoChunkRange (ChunkPoint (DataUID <$> _startUid range) (_startOffset range)) (ChunkPoint (DataUID <$> _endUid range) (_endOffset range))
                                let patchFromClient = PatchFromClient vdocChunk (Just protoChunkRange)
                                addPatch patchKey patchFromClient $ \case
                                    (Left(_, msg)) -> handleError msg
                                    (Right _patch) -> return []
-}
    _ -> return ()


handleError :: String -> IO [SomeStoreAction]
handleError msg = do
            print msg
            return []

refineStore :: ReactStore GlobalState
refineStore = mkStore $ GlobalState Nothing Nothing 0 (MarkPositions M.empty) Desktop (Nothing, Nothing)

dispatch :: RefineAction -> [SomeStoreAction]
dispatch a = [SomeStoreAction refineStore a]


-- (no idea if there are char encoding issues here.  but it's probably safe to use it for development.)
consoleLog :: ToJSON a => JSString -> a -> IO ()
consoleLog str state = consoleLog_ str ((pack . cs . encode) state)

foreign import javascript unsafe
    "window.getSelection().rangeCount > 0 \
    \&& !(!window.getSelection().getRangeAt(0)) \
    \&& !window.getSelection().getRangeAt(0).collapsed"
    js_hasRange :: IO Bool

getRange :: IO (Maybe Range)
getRange = (AE.decode . cs . unpack) <$> js_getRange

-- TODO remove the default values for os and oe
foreign import javascript unsafe
    "(function (range) { \
    \   var result = { os : 0, oe : 0 }; \
    \   if(range.startContainer.parentElement.attributes['data-uid']) { \
    \       result.ns = parseInt(range.startContainer.parentElement.attributes['data-uid'].value, 10); \
    \       result.os = range.startOffset; \
    \   } \
    \   if(range.endContainer.parentElement.attributes['data-uid']) { \
    \       result.ne = parseInt(range.endContainer.parentElement.attributes['data-uid'].value, 10); \
    \       result.oe = range.endOffset; \
    \   } \
    \   result.top = range.startContainer.parentElement.getBoundingClientRect().top; \
    \   result.bottom = range.endContainer.parentElement.getBoundingClientRect().bottom; \
    \   result.scrollOffset = typeof( window.pageYOffset ) == 'number' && window.pageYOffset \
    \                         || document.body && document.body.scrollTop \
    \                         || document.documentElement && document.documentElement.scrollTop; \
    \   return JSON.stringify(result); \
    \}) (window.getSelection().getRangeAt(0))"
    js_getRange :: IO JSString

foreign import javascript unsafe
  -- see webpack.config.js for a definition of the environment variable.
  "if( process.env.IS_IN_WEBPACK ){ console.log($1, JSON.parse($2)); }"
  consoleLog_ :: JSString -> JSString -> IO ()
