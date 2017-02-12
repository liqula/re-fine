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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Store where

import           Control.Lens ((&), (^.), (%~), to)
import qualified Data.Aeson as AE
import           Data.Aeson (ToJSON, encode)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.String.Conversions
import           Data.JSString (JSString, pack, unpack)
import           React.Flux

import Refine.Common.Types (CompositeVDoc(..))
import qualified Refine.Common.Types as RT

import           Refine.Common.VDoc.HTML (insertMoreMarks)
import           Refine.Frontend.Bubbles.Store (bubblesStateUpdate)
import           Refine.Frontend.Bubbles.Types
import           Refine.Frontend.Rest
import           Refine.Frontend.Screen.Store (screenStateUpdate)
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Test.Samples
import           Refine.Frontend.Types


-- TODO: move to Screen.Calculations
toSize :: Int -> WindowSize
toSize sz
  | sz <= 480  = Mobile
  | sz <= 1024 = Tablet
  | otherwise  = Desktop

instance StoreData GlobalState where
    type StoreAction GlobalState = RefineAction
    transform action state = do
        consoleLog "Old state: " state
        consoleLogStringified "Action: " action

        emitBackendCallsFor action state

        transformedAction <- case action of
            TriggerUpdateSelection deviceOffset -> do
                -- for efficiency reasons, only ask JS when we get this action
                hasRange <- js_hasRange
                range <- if hasRange then getRange else pure Nothing
                pure . BubblesAction $ UpdateSelection (range, Just deviceOffset)
            _ -> pure action


        let newState = state
              & gsVDoc                     %~ vdocUpdate transformedAction
              & gsVDocList                 %~ vdocListUpdate transformedAction
              & gsBubblesState             %~ bubblesStateUpdate transformedAction
              & gsScreenState              %~ screenStateUpdate transformedAction

        consoleLog "New state: " newState
        pure newState


vdocUpdate :: RefineAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action state = case action of
    OpenDocument openedVDoc
      -> Just openedVDoc

    AddDiscussion discussion
      -> case state of
        Nothing   -> Nothing -- no vdoc: we cannot put the comment anywhere
                             -- FIXME: i think this should be an error. ~fisx
        Just vdoc -> Just $ vdoc
          & RT.compositeVDocDiscussions
              %~ M.insert (discussion ^. RT.compositeDiscussion . RT.discussionID) discussion
          & RT.compositeVDocVersion
              %~ insertMoreMarks [discussion ^. RT.compositeDiscussion . RT.discussionRange]

    AddNote note
      -> case state of
        Nothing   -> Nothing -- no vdoc: we cannot put the note anywhere
                             -- FIXME: i think this should be an error. ~fisx
        Just vdoc -> Just $ vdoc
          & RT.compositeVDocNotes
              %~ M.insert (note ^. RT.noteID) note
          & RT.compositeVDocVersion
              %~ insertMoreMarks [note ^. RT.noteRange]

    _ -> state

vdocListUpdate :: RefineAction -> Maybe [RT.ID RT.VDoc] -> Maybe [RT.ID RT.VDoc]
vdocListUpdate action state = case action of
    LoadedDocumentList list -> Just list
    _ -> state

emitBackendCallsFor :: RefineAction -> GlobalState -> IO ()
emitBackendCallsFor action state = case action of
    LoadDocumentList -> do
        listVDocs $ \case
            (Left(_, msg)) -> handleError msg
            (Right loadedVDocs) -> pure . dispatch $ LoadedDocumentList ((^. RT.vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left(_, msg)) -> handleError msg
            (Right loadedVDoc) -> pure . dispatch $ OpenDocument loadedVDoc

    AddDemoDocument -> do
        createVDoc (RT.CreateVDoc sampleTitle sampleAbstract sampleText) $ \case
            (Left(_, msg)) -> handleError msg
            (Right loadedVDoc) -> pure . dispatch $ OpenDocument loadedVDoc

    BubblesAction (SubmitComment text category forRange) -> do
      -- here we need to distinguish which comment category we want to submit
      -- check the state and what the user selected there
      -- (FIXME: the new correct technical term for 'category' is 'kind'.)
      case category of
        Just Discussion ->
          addDiscussion (state ^. gsVDoc . to fromJust . RT.compositeVDocRepo . RT.vdocHeadEdit)
                     (RT.CreateDiscussion text True (createChunkRange forRange)) $ \case
            (Left(_, msg)) -> handleError msg
            (Right discussion) -> pure $ dispatch (AddDiscussion discussion)
        Just Note ->
          addNote (fromJust (state ^. gsVDoc) ^. RT.compositeVDocRepo ^. RT.vdocHeadEdit)
                     (RT.CreateNote text True (createChunkRange forRange)) $ \case
            (Left(_, msg)) -> handleError msg
            (Right note) -> pure $ dispatch (AddNote note)
        Nothing -> pure ()

    CreateUser createUserData -> do
      createUser createUserData $ \case
        (Left (_, msg)) -> handleError msg
        (Right u) -> do
          print u
          pure $ dispatch LoadDocumentList

    Login loginData -> do
      login loginData $ \case
        (Left(_, msg)) -> handleError msg
        (Right ()) -> do
          print @String "logged in"
          pure $ dispatch LoadDocumentList

    Logout -> do
      logout $ \case
        (Left(_, msg)) -> handleError msg
        (Right ()) -> do
          print @String "logged out"
          pure $ dispatch LoadDocumentList

    _ -> pure ()

{- TODO submitting an edit does not work yet
    SubmitEdit -> do
        let vdocId = _metaKey . _vdocMeta . fromJust $ _vdoc state
        let editId = _vdocHead . fromJust $ _vdoc state
        let vdocChunk = VDocChunk "<p><strong>This is my new and obviously much, much better text :-)</strong></p>"
        let maybeRange = _currentSelection state
        let editKey = EditKey vdocId editId
        case maybeRange of
            (Nothing, _)    -> pure ()
            (Just range, _) -> do
                                let protoChunkRange = ProtoChunkRange (ChunkPoint (DataUID <$> _startUid range) (_startOffset range)) (ChunkPoint (DataUID <$> _endUid range) (_endOffset range))
                                let editFromClient = EditFromClient vdocChunk (Just protoChunkRange)
                                addEdit editKey editFromClient $ \case
                                    (Left(_, msg)) -> handleError msg
                                    (Right _edit) -> pure []
-}

createChunkRange :: Maybe Range -> RT.CreateChunkRange
createChunkRange Nothing = RT.CreateChunkRange Nothing Nothing
createChunkRange (Just range) = RT.CreateChunkRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)

handleError :: String -> IO [SomeStoreAction]
handleError msg = do
            print msg
            pure []

refineStore :: ReactStore GlobalState
refineStore = mkStore emptyGlobalState

dispatch :: RefineAction -> [SomeStoreAction]
dispatch a = [SomeStoreAction refineStore a]


foreign import javascript unsafe
    "window.getSelection().rangeCount > 0 \
    \&& !(!window.getSelection().getRangeAt(0)) \
    \&& !window.getSelection().getRangeAt(0).collapsed"
    js_hasRange :: IO Bool

getRange :: IO (Maybe Range)
getRange = (AE.decode . cs . unpack) <$> js_getRange

foreign import javascript unsafe
    "refine$getSelectionRange()"
    js_getRange :: IO JSString


-- * ad hoc logging.

-- FUTUREWORK: we should probably find a way to avoid rendering all these json values into strings
-- in the production code.

-- (no idea if there are char encoding issues here.  but it's probably safe to use it for development.)
consoleLog :: ToJSON a => JSString -> a -> IO ()
consoleLog str state = consoleLog_ str ((pack . cs . encode) state)

consoleLogStringified :: ToJSON a => JSString -> a -> IO ()
consoleLogStringified str state = js_consoleLog str ((pack . cs . encode) state)

foreign import javascript unsafe
  -- see webpack.config.js for a definition of the environment variable.

  "if( process.env.NODE_ENV === 'development' ){ \
  \    try { \
  \        console.log($1, JSON.parse($2)); \
  \    } catch(e) { \
  \        console.log($1, '*** ERROR in Refine.Frontend.Store.consoleLog_ (see gitlab issue #134; make sure node ==v6.)', e); \
  \    } \
  \}"
  consoleLog_ :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "if( process.env.NODE_ENV === 'development' ){ console.log($1, $2); }"
  js_consoleLog :: JSString -> JSString -> IO ()
