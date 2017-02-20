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

import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Control.Lens ((&), (^.), (^?), (%~), to)
import qualified Data.Aeson as AE
import           Data.Aeson (ToJSON, encode)
import           Data.JSString (JSString, pack, unpack)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.String.Conversions
import           React.Flux

import Refine.Common.Types (CompositeVDoc(..), Contribution(..))
import qualified Refine.Common.Types as RT

import qualified Refine.Common.VDoc.HTML as Common
import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Store (headerStateUpdate)
import           Refine.Frontend.MainMenu.Store (mainMenuUpdate)
import           Refine.Frontend.Login.Store (loginStateUpdate)
import           Refine.Frontend.Login.Types
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
    transform ClearState _ = pure emptyGlobalState  -- for testing only!
    transform action state = do
        consoleLog "Old state: " state
        consoleLogStringified "Action: " action

        emitBackendCallsFor action state

        transformedAction <- case action of
            TriggerUpdateSelection releasePositionOnPage toolbarStatus -> do
                -- for efficiency reasons, only ask JS when we get this action
                hasRange <- js_hasRange
                mrange <- if hasRange then getRange else pure Nothing
                pure . ContributionAction $ UpdateSelection
                    (case mrange of
                        Nothing -> NothingSelectedButUpdateTriggered releasePositionOnPage
                        Just range -> RangeSelected range releasePositionOnPage)
                    toolbarStatus

            ContributionAction (ShowCommentEditor _) -> do
                js_removeAllRanges >> pure action

            _ -> pure action

        let newState = state
              & gsVDoc                       %~ vdocUpdate transformedAction
              & gsVDocList                   %~ vdocListUpdate transformedAction
              & gsContributionState          %~ maybe id contributionStateUpdate (transformedAction ^? _ContributionAction)
              & gsHeaderState                %~ headerStateUpdate transformedAction
              & gsScreenState                %~ screenStateUpdate transformedAction
              & gsNotImplementedYetIsVisible %~ notImplementedYetIsVisibleUpdate transformedAction
              & gsLoginState                 %~ loginStateUpdate transformedAction
              & gsMainMenuState              %~ mainMenuUpdate transformedAction
              & gsToolbarSticky              %~ toolbarStickyUpdate transformedAction

        consoleLog "New state: " newState
        pure newState


vdocUpdate :: RefineAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action Nothing = case action of
    OpenDocument openedVDoc -> Just openedVDoc
    _ -> Nothing

vdocUpdate action (Just vdoc) = Just $ case action of
    AddDiscussion discussion
      -> vdoc
          & RT.compositeVDocDiscussions
              %~ M.insert (discussion ^. RT.compositeDiscussion . RT.discussionID) discussion
          & RT.compositeVDocVersion
              %~ Common.insertMoreMarks [ContribDiscussion $ discussion ^. RT.compositeDiscussion]

    AddNote note
      -> vdoc
          & RT.compositeVDocNotes
              %~ M.insert (note ^. RT.noteID) note
          & RT.compositeVDocVersion
              %~ Common.insertMoreMarks [ContribNote note]

    ContributionAction (ShowCommentEditor (Just range))
      -> vdoc & RT.compositeVDocVersion %~ Common.highlightRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)
    ContributionAction HideCommentEditor
      -> vdoc & RT.compositeVDocVersion %~ Common.removeHighlights

    _ -> vdoc


vdocListUpdate :: RefineAction -> Maybe [RT.ID RT.VDoc] -> Maybe [RT.ID RT.VDoc]
vdocListUpdate action state = case action of
    LoadedDocumentList list -> Just list
    _ -> state

notImplementedYetIsVisibleUpdate :: RefineAction -> Bool -> Bool
notImplementedYetIsVisibleUpdate action state = case action of
  ShowNotImplementedYet -> True
  HideNotImplementedYet -> False
  _                 -> state

toolbarStickyUpdate :: RefineAction -> Bool -> Bool
toolbarStickyUpdate action state = case action of
  ToolbarStickyStateChange state' -> state'
  _                               -> state

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

    ContributionAction (SubmitComment text category forRange) -> do
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
          addNote (state ^. gsVDoc . to fromJust . RT.compositeVDocRepo . RT.vdocHeadEdit)
                     (RT.CreateNote text True (createChunkRange forRange)) $ \case
            (Left(_, msg)) -> handleError msg
            (Right note) -> pure $ dispatch (AddNote note)
        Nothing -> pure ()

    CreateUser createUserData -> do
      createUser createUserData $ \case
        (Left (_, msg)) -> handleError msg
        (Right _user) -> do
          pure $ dispatch LoadDocumentList

    Login loginData -> do
      login loginData $ \case
        (Left(_, msg)) -> handleError msg
        (Right username) -> do
          pure $ dispatch (ChangeCurrentUser $ LoggedInUser username) <>
                 dispatch LoadDocumentList

    Logout -> do
      logout $ \case
        (Left(_, msg)) -> handleError msg
        (Right ()) -> do
          pure $ dispatch (ChangeCurrentUser NonLoggedInUser) <>
                 dispatch LoadDocumentList

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

createChunkRange :: Maybe Range -> RT.ChunkRange
createChunkRange Nothing = RT.ChunkRange Nothing Nothing
createChunkRange (Just range) = RT.ChunkRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)

handleError :: String -> IO [SomeStoreAction]
handleError msg = do
            consoleLog "handleError" msg
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


-- * ugly hacks

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.
reactFluxWorkAroundForkIO :: IO () -> IO ThreadId
reactFluxWorkAroundForkIO = forkIO

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.  Try to increase microseconds if you still experience race conditions.
reactFluxWorkAroundThreadDelay :: IO ()
reactFluxWorkAroundThreadDelay = threadDelay 10000


-- * pretty hacks (:

foreign import javascript unsafe
  "window.getSelection().removeAllRanges();"
  js_removeAllRanges :: IO ()
