{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Store where

import           Control.Lens ((&), (^.), (%~))
import qualified Data.Aeson as AE
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
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

        selectionAction <- case action of -- for efficiency reasons, don't ask JS on each action
            SetSelection deviceOffset -> do
                hasRange <- js_hasRange
                range <- if hasRange then getRange else return Nothing
                return $ UpdateSelection (range, Just deviceOffset)
            _ -> return action


        let newState = state
              & gsVDoc                     %~ vdocUpdate action
              & gsVDocList                 %~ vdocListUpdate action
              & gsHeaderHeight             %~ headerHeightUpdate action
              & gsMarkPositions            %~ markPositionsUpdate action
              & gsWindowSize               %~ windowSizeUpdate action
              & gsCurrentSelection         %~ currentSelectionUpdate selectionAction
              & gsCommentIsVisible         %~ commentIsVisibleUpdate action
              & gsCommentEditorIsVisible   %~ commentEditorIsVisibleUpdate action

        consoleLog "New state: " newState
        return newState


vdocUpdate :: RefineAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action state = case action of
    OpenDocument openedVDoc -> Just openedVDoc
    AddComment comment      -> case state of
        Nothing   -> Nothing -- no vdoc: we cannot put the comment anywhere
        Just vdoc -> Just $ vdoc { _compositeVDocComments = comment : vdoc ^. compositeVDocComments }
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

currentSelectionUpdate :: RefineAction -> Selection -> Selection
currentSelectionUpdate action state = case action of
    UpdateSelection newState -> newState
    ClearSelection -> (Nothing, Nothing)
    SubmitPatch    -> (Nothing, Nothing)
    _ -> state

commentIsVisibleUpdate :: RefineAction -> Bool -> Bool
commentIsVisibleUpdate action state = case action of
    ShowComment -> True
    HideComment -> False
    _ -> state

commentEditorIsVisibleUpdate :: RefineAction -> (Bool, Maybe Range) -> (Bool, Maybe Range)
commentEditorIsVisibleUpdate action state = case action of
    ShowCommentEditor curSelection -> (True, curSelection)
    HideCommentEditor -> (False, Nothing)
    _ -> state



emitBackendCallsFor :: RefineAction -> GlobalState -> IO ()
emitBackendCallsFor action state = case action of
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

    SubmitComment text _category forRange -> do  -- later we will have 3 cases, depending on the category. But first the backend needs to get sorted on the types.
      addComment (fromJust (state ^. gsVDoc) ^. compositeVDocRepo ^. vdocHeadPatch)
                 (CreateComment text True (createChunkRange forRange)) $ \case
        (Left(_, msg)) -> handleError msg
        (Right comment) -> return . dispatch $ AddComment comment

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


createChunkRange :: Maybe Range -> CreateChunkRange
createChunkRange Nothing = CreateChunkRange Nothing Nothing
createChunkRange (Just range) = CreateChunkRange (range ^. startPoint) (range ^. endPoint)

handleError :: String -> IO [SomeStoreAction]
handleError msg = do
            print msg
            return []

refineStore :: ReactStore GlobalState
refineStore = mkStore $ GlobalState Nothing Nothing 0 (MarkPositions M.empty) Desktop (Nothing, Nothing) False (False, Nothing)

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
    \   var result = {}; \
    \   if(range.startContainer.parentElement.attributes['data-uid']) { \
    \       result.start = { \
    \         node: parseInt(range.startContainer.parentElement.attributes['data-uid'].value, 10), \
    \         offset: range.startOffset + parseInt(range.startContainer.parentElement.attributes['data-offset'].value, 10) \
    \       }; \
    \   } \
    \   if(range.endContainer.parentElement.attributes['data-uid']) { \
    \       result.end = { \
    \         node: parseInt(range.endContainer.parentElement.attributes['data-uid'].value, 10), \
    \         offset: range.endOffset + parseInt(range.endContainer.parentElement.attributes['data-offset'].value, 10) \
    \       };\
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
