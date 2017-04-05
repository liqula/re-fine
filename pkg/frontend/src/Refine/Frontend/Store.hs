{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Store where

import           Control.Concurrent (forkIO, yield, threadDelay)
import           Control.Lens (_Just, (&), (^.), (^?), (^?!), (%~), to)
import           Control.Monad.IO.Class
import           Control.Monad.State.Class (MonadState, modify)
import           Control.Monad.Trans.State (StateT, runStateT)
import           Control.Monad (void, when)
import           Data.Aeson (decode, eitherDecode, toJSON, Value(String))
import           Data.JSString (JSString, unpack)
import qualified Data.Map.Strict as M
import           Data.String.Conversions
import           React.Flux

import           Refine.Common.Types (CompositeVDoc(..), Contribution(..))
import qualified Refine.Common.Types as C
import qualified Refine.Common.VDoc.HTML as C
import           Refine.Common.Rest (ApiError(..))
import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Store (documentStateUpdate, editorStateToVDocVersion)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Store (headerStateUpdate)
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Login.Store (loginStateUpdate)
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Store (mainMenuUpdate)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Rest
import           Refine.Frontend.Screen.Store (screenStateUpdate)
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console
import           Refine.Frontend.Test.Samples
import           Refine.Frontend.Translation.Store (translationsUpdate)
import           Refine.Frontend.Types


instance StoreData GlobalState where
    type StoreAction GlobalState = GlobalAction
    transform = loop . (:[])
      where
        loop :: [GlobalAction] -> GlobalState -> IO GlobalState
        loop [] state = pure state
        loop (action : actions) state = do
          (state', actions') <- runStateT (transformGlobalState @Transform action state) []
          loop (actions <> actions') state'

type MonadTransform m = (Functor m, Applicative m, Monad m, MonadIO m, MonadState [GlobalAction] m)
type Transform = StateT [GlobalAction] IO

-- | FUTUREWORK: have more fine-grained constraints on 'm'.
transformGlobalState :: forall m. MonadTransform m => GlobalAction -> GlobalState -> m GlobalState
transformGlobalState = transf
  where
    transf :: GlobalAction -> GlobalState -> m GlobalState
    transf ClearState _ = pure emptyGlobalState  -- for testing only!
    transf action state = do
        consoleLogJSONM "Old state: " state
        consoleLogJSStringM "Action: " (cs $ show action)

        -- all updates to state are pure!
        let state' = pureTransform action state

        -- ajax
        liftIO $ emitBackendCallsFor action state

        -- other effects
        case action of
            ContributionAction (TriggerUpdateRange releasePositionOnPage) -> do
                reDispatchM . ContributionAction . maybe ClearRange SetRange
                    =<< getRange releasePositionOnPage
                removeAllRanges  -- (See also: calls to 'C.highlightRange', 'C.removeHighlights' below.)

                when (state ^. gsHeaderState . hsToolbarExtensionStatus == CommentToolbarExtensionWithRange) $ do
                  -- (if the comment editor (or dialog) is started via the toolbar extension, this
                  -- is where it should be started.)
                  reDispatchM $ ContributionAction ShowCommentEditor

            ContributionAction (ScheduleAddMarkPosition _ _) -> do
                let nothingScheduled = M.null $ state ^. gsContributionState . csMarkPositions . markPositionsScheduled
                    delayMiliSecs = 150
                when nothingScheduled . void . liftIO . forkIO $ do
                  threadDelay $ delayMiliSecs * 1000
                  dispatchAndExec $ ContributionAction DischargeAddMarkPositions

            _ -> pure ()

        consoleLogJSONM "New state: " $ if state' /= state then toJSON state' else (String "[UNCHANGED]" :: Value)
        pure state'

    pureTransform :: GlobalAction -> GlobalState -> GlobalState
    pureTransform action state = state
      & gsVDoc                       %~ vdocUpdate action
      & gsVDocList                   %~ vdocListUpdate action
      & gsContributionState          %~ contributionStateUpdate action
      & gsHeaderState                %~ headerStateUpdate action
      & gsDocumentState              %~ documentStateUpdate action (state ^? gsVDoc . _Just . C.compositeVDocVersion)
      & gsScreenState                %~ maybe id screenStateUpdate (action ^? _ScreenAction)
      & gsNotImplementedYetIsVisible %~ notImplementedYetIsVisibleUpdate action
      & gsLoginState                 %~ loginStateUpdate action
      & gsMainMenuState              %~ mainMenuUpdate action
      & gsToolbarSticky              %~ toolbarStickyUpdate action
      & gsTranslations               %~ translationsUpdate action


-- * pure updates

vdocUpdate :: GlobalAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action Nothing = case action of
    OpenDocument openedVDoc -> Just openedVDoc
    _ -> Nothing

vdocUpdate action (Just vdoc) = Just $ case action of
    AddDiscussion discussion
      -> vdoc
          & C.compositeVDocDiscussions
              %~ M.insert (discussion ^. C.compositeDiscussion . C.discussionID) discussion
          & C.compositeVDocVersion
              %~ C.insertMoreMarks [ContribDiscussion $ discussion ^. C.compositeDiscussion]

    AddNote note
      -> vdoc
          & C.compositeVDocNotes
              %~ M.insert (note ^. C.noteID) note
          & C.compositeVDocVersion
              %~ C.insertMoreMarks [ContribNote note]

    AddEdit edit
      -> vdoc
          & C.compositeVDocEdits
              %~ M.insert (edit ^. C.editID) edit
          & C.compositeVDocVersion
              %~ C.insertMoreMarks [ContribEdit edit]

    ContributionAction (SetRange range)
      -> vdoc & C.compositeVDocVersion %~ C.removeHighlights  -- FIXME: this should be implicit in highlightRange
              & C.compositeVDocVersion %~ C.highlightRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)
    ContributionAction ClearRange
      -> vdoc & C.compositeVDocVersion %~ C.removeHighlights

    _ -> vdoc


vdocListUpdate :: GlobalAction -> Maybe [C.ID C.VDoc] -> Maybe [C.ID C.VDoc]
vdocListUpdate action state = case action of
    LoadedDocumentList list -> Just list
    _ -> state


notImplementedYetIsVisibleUpdate :: GlobalAction -> Bool -> Bool
notImplementedYetIsVisibleUpdate action state = case action of
  ShowNotImplementedYet -> True
  HideNotImplementedYet -> False
  _                 -> state


toolbarStickyUpdate :: GlobalAction -> Bool -> Bool
toolbarStickyUpdate action state = case action of
  ToolbarStickyStateChange state' -> state'
  _                               -> state


-- * ajax

emitBackendCallsFor :: GlobalAction -> GlobalState -> IO ()
emitBackendCallsFor action state = case action of

    -- documents

    LoadDocumentList -> do
        listVDocs $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDocs) -> dispatchM $ LoadedDocumentList ((^. C.vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDoc) -> dispatchM $ OpenDocument loadedVDoc


    -- contributions

    ContributionAction (SubmitComment text kind) -> do
      let forRange :: C.ChunkRange
          forRange = state ^. gsContributionState . csCurrentRange . to createChunkRange
      case kind of
        Just CommentKindDiscussion ->
          addDiscussion (state ^?! gsVDoc . _Just . C.compositeVDocRepo . C.vdocHeadEdit)
                     (C.CreateDiscussion text True forRange) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right discussion) -> dispatchM $ AddDiscussion discussion
        Just CommentKindNote ->
          addNote (state ^?! gsVDoc . _Just . C.compositeVDocRepo . C.vdocHeadEdit)
                     (C.CreateNote text True forRange) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right note) -> dispatchM $ AddNote note
        Nothing -> pure ()

    DocumentAction DocumentEditSave -> case state ^? gsDocumentState . documentStateEdit of
      Just estate@(editorStateToVDocVersion -> Right newvers) -> do
        let eid :: C.ID C.Edit
            eid = state ^?! gsVDoc . _Just . C.compositeVDocEditID

            cid :: C.Create C.Edit
            cid = C.CreateEdit
                  { C._createEditDesc  = "..."                          -- TODO: #233
                  , C._createEditRange = state ^. gsContributionState . csCurrentRange . to createChunkRange
                  , C._createEditVDoc  = C.downgradeVDocVersionWR newvers
                  , C._createEditKind  = estate ^. documentEditStateKind
                  , C._createEditMotiv = "..."                          -- TODO: #233
                  }

        addEdit eid cid $ \case
          Left rsp   -> handleError rsp (const [])
          Right edit -> dispatchM $ AddEdit edit

      bad -> let msg = "DocumentAction DocumentEditSave: "
                    <> "not in editor state or content cannot be converted to html."
                    <> show bad
             in gracefulError msg $ pure ()


    -- i18n

    LoadTranslations locate -> do
      getTranslations (C.GetTranslations locate) $ \case
        (Left rsp) -> handleError rsp (const [])
        (Right l10) -> do
          dispatchM $ ChangeTranslations l10


    -- users

    CreateUser createUserData -> do
      createUser createUserData $ \case
        (Left rsp)    -> do
          handleError rsp $ \case
            ApiUserCreationError e -> [MainMenuAction $ MainMenuActionRegistrationError e]
            _                      -> []

        (Right _user) -> do
          dispatchManyM
            [ MainMenuAction $ MainMenuActionOpen MainMenuLogin
            , MainMenuAction MainMenuActionClearErrors
            ]

    Login loginData -> do
      login loginData $ \case
        (Left rsp) -> handleError rsp $ \case
          ApiUserNotFound e -> [MainMenuAction . MainMenuActionLoginError $ "could not login as " <> e]
          _                 -> []

        (Right username) -> do
          dispatchManyM
            [ ChangeCurrentUser $ UserLoggedIn username
            , MainMenuAction MainMenuActionClose
            ]

    Logout -> do
      logout $ \case
        (Left rsp) -> handleError rsp (const [])
        (Right ()) -> do
          dispatchManyM
            [ ChangeCurrentUser UserLoggedOut
            , MainMenuAction MainMenuActionClose
            ]


    -- testing & dev

    AddDemoDocument -> do
        createVDoc (C.CreateVDoc sampleTitle sampleAbstract sampleText) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDoc) -> dispatchM $ OpenDocument loadedVDoc


    -- default

    _ -> pure ()


createChunkRange :: Maybe Range -> C.ChunkRange
createChunkRange Nothing = C.ChunkRange Nothing Nothing
createChunkRange (Just range) = C.ChunkRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)

handleError :: (Int, String) -> (ApiError -> [GlobalAction]) -> IO [SomeStoreAction]
handleError (code, rsp) onApiError = case eitherDecode $ cs rsp of
  Left err -> do
    consoleLogJSStringM "handleError: backend sent invalid response: " . cs $ unwords [show code, rsp, err]
        -- FIXME: use 'gracefulError' here.  (rename 'gracefulError' to 'assertContract'?)
    pure []
  Right apiError -> do
    consoleLogJSStringM "handleApiError" . cs $ show apiError
    pure . mconcat . fmap dispatch $ onApiError apiError


-- * triggering actions

-- FIXME: return a single some-action, not a list?
dispatch :: GlobalAction -> ViewEventHandler
dispatch a = [someStoreAction @GlobalState a]

dispatchMany :: [GlobalAction] -> ViewEventHandler
dispatchMany = mconcat . fmap dispatch

dispatchM :: Monad m => GlobalAction -> m ViewEventHandler
dispatchM = pure . dispatch

dispatchManyM :: Monad m => [GlobalAction] -> m ViewEventHandler
dispatchManyM = pure . dispatchMany

reDispatchM :: MonadState [GlobalAction] m => GlobalAction -> m ()
reDispatchM a = reDispatchManyM [a]

reDispatchManyM :: MonadState [GlobalAction] m => [GlobalAction] -> m ()
reDispatchManyM as = modify (<> as)

dispatchAndExec :: MonadIO m => GlobalAction -> m ()
dispatchAndExec a = liftIO . reactFluxWorkAroundForkIO $ do
  () <- executeAction `mapM_` dispatch a
  pure ()

dispatchAndExecMany :: MonadIO m => [GlobalAction] -> m ()
dispatchAndExecMany as = liftIO . reactFluxWorkAroundForkIO $ do
  () <- executeAction `mapM_` dispatchMany as
  pure ()


-- * ranges and selections

getRange :: MonadIO m => OffsetFromDocumentTop -> m (Maybe Range)
getRange pos = liftIO $ decode . cs . unpack <$> js_getRange pos

foreign import javascript unsafe
  "refine$getSelectionRange($1)"
  js_getRange :: OffsetFromDocumentTop -> IO JSString

removeAllRanges :: MonadIO m => m ()
removeAllRanges = liftIO js_removeAllRanges

foreign import javascript unsafe
  "window.getSelection().removeAllRanges();"
  js_removeAllRanges :: IO ()


-- * work-arounds for known bugs.

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.
reactFluxWorkAroundForkIO :: IO () -> IO ()
reactFluxWorkAroundForkIO action = void . forkIO $ yield >> action

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.  Try to increase microseconds if you still experience race conditions.
reactFluxWorkAroundThreadDelay :: IO ()
reactFluxWorkAroundThreadDelay = threadDelay 10000
