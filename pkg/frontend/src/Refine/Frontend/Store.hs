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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Store where

import           Control.Concurrent (forkIO, yield, threadDelay)
import           Control.Lens (_Just, (&), (^.), (^?), (^?!), (%~), to)
import           Control.Monad (void)
import qualified Data.Aeson as AE
import           Data.JSString (JSString, unpack)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.String.Conversions
import           React.Flux

import           Refine.Common.Types (CompositeVDoc(..), Contribution(..))
import qualified Refine.Common.Types as RT
import qualified Refine.Common.VDoc.HTML as Common
import           Refine.Common.Rest (ApiError(..))

import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Store (documentStateUpdate, editorStateToVDocVersion)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Store (headerStateUpdate)
import           Refine.Frontend.Login.Store (loginStateUpdate)
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Store (mainMenuUpdate)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Rest
import           Refine.Frontend.Screen.Store (screenStateUpdate)
import           Refine.Frontend.Test.Console
import           Refine.Frontend.Test.Samples
import           Refine.Frontend.Translation.Store (translationsUpdate)
import           Refine.Frontend.Types


instance StoreData GlobalState where
    type StoreAction GlobalState = GlobalAction
    transform ClearState _ = pure emptyGlobalState  -- for testing only!
    transform action state = do
        consoleLogJSONM "Old state: " state
        consoleLogJSStringM "Action: " (cs $ show action)

        emitBackendCallsFor action state

        action' <- case action of
            TriggerUpdateSelection releasePositionOnPage toolbarStatus -> do
                mrange <- getRange
                pure . ContributionAction $ UpdateSelection
                    (case mrange of
                        Nothing -> NothingSelectedButUpdateTriggered releasePositionOnPage
                        Just range -> RangeSelected range releasePositionOnPage)
                    toolbarStatus

            ContributionAction (ShowCommentEditor _) -> do
                js_removeAllRanges >> pure action

            _ -> pure action

        let state' = state
              & gsVDoc                       %~ vdocUpdate action'
              & gsVDocList                   %~ vdocListUpdate action'
              & gsContributionState          %~ maybe id contributionStateUpdate (action' ^? _ContributionAction)
              & gsHeaderState                %~ headerStateUpdate action'
              & gsDocumentState              %~ documentStateUpdate action' (state ^? gsVDoc . _Just . RT.compositeVDocVersion)
              & gsScreenState                %~ maybe id screenStateUpdate (action' ^? _ScreenAction)
              & gsNotImplementedYetIsVisible %~ notImplementedYetIsVisibleUpdate action'
              & gsLoginState                 %~ loginStateUpdate action'
              & gsMainMenuState              %~ mainMenuUpdate action'
              & gsToolbarSticky              %~ toolbarStickyUpdate action'
              & gsTranslations               %~ fmap (translationsUpdate action')

        consoleLogJSONM "New state: " state'
        pure state'

vdocUpdate :: GlobalAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
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

    AddEdit edit
      -> vdoc
          & RT.compositeVDocEdits
              %~ M.insert (edit ^. RT.editID) edit
          & RT.compositeVDocVersion
              %~ Common.insertMoreMarks [ContribEdit edit]

    ContributionAction (ShowCommentEditor (Just range))
      -> vdoc & RT.compositeVDocVersion %~ Common.highlightRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)
    ContributionAction HideCommentEditor
      -> vdoc & RT.compositeVDocVersion %~ Common.removeHighlights

    _ -> vdoc


vdocListUpdate :: GlobalAction -> Maybe [RT.ID RT.VDoc] -> Maybe [RT.ID RT.VDoc]
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

emitBackendCallsFor :: GlobalAction -> GlobalState -> IO ()
emitBackendCallsFor action state = case action of
    LoadDocumentList -> do
        listVDocs $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDocs) -> pure . dispatch $ LoadedDocumentList ((^. RT.vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDoc) -> pure . dispatch $ OpenDocument loadedVDoc

    AddDemoDocument -> do
        createVDoc (RT.CreateVDoc sampleTitle sampleAbstract sampleText) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDoc) -> pure . dispatch $ OpenDocument loadedVDoc

    ContributionAction (SubmitComment text category forRange) -> do
      -- here we need to distinguish which comment category we want to submit
      -- check the state and what the user selected there
      -- (FIXME: the new correct technical term for 'category' is 'kind'.)
      case category of
        Just Discussion ->
          addDiscussion (state ^. gsVDoc . to fromJust . RT.compositeVDocRepo . RT.vdocHeadEdit)
                     (RT.CreateDiscussion text True (createChunkRange forRange)) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right discussion) -> pure $ dispatch (AddDiscussion discussion)
        Just Note ->
          addNote (state ^. gsVDoc . to fromJust . RT.compositeVDocRepo . RT.vdocHeadEdit)
                     (RT.CreateNote text True (createChunkRange forRange)) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right note) -> pure $ dispatch (AddNote note)
        Nothing -> pure ()

    DocumentAction DocumentEditSave -> do
      let eid :: RT.ID RT.Edit
          eid = state ^?! gsVDoc . _Just . RT.compositeVDocEditID

          adHocFail msg = error $ "DocumentAction DocumentEditSave: " <> msg  -- TODO: #255

      estate :: EditorState
        <- case state ^. gsDocumentState of
             DocumentStateEdit s -> pure s
             DocumentStateView   -> adHocFail "document state is in 'view'"

      newvers :: RT.VDocVersion 'RT.HTMLWithMarks
        <- either (adHocFail . ("newvers cannot be parsed: " <>)) pure $ editorStateToVDocVersion estate

      let cid :: RT.Create RT.Edit
          cid = RT.CreateEdit
                  { RT._createEditDesc  = "..."                          -- TODO: #233
                  , RT._createEditRange = RT.ChunkRange Nothing Nothing  -- FIXME: 'state ^. gsContributionState . csCurrentSelection'
                  , RT._createEditVDoc  = Common.downgradeVDocVersionWR newvers
                  , RT._createEditKind  = estate ^. editorStateKind
                  , RT._createEditMotiv = "..."                          -- TODO: #233
                  }

      addEdit eid cid $ \case
        Left rsp   -> handleError rsp (const [])
        Right edit -> pure $ dispatch (AddEdit edit)

    CreateUser createUserData -> do
      createUser createUserData $ \case
        (Left rsp)    -> do
          handleError rsp $ \case
            ApiUserCreationError e -> [MainMenuAction $ MainMenuActionRegistrationError e]
            _                      -> []

        (Right _user) -> do
          pure $ dispatchMany
            [ MainMenuAction $ MainMenuActionOpen MainMenuLogin
            , MainMenuAction MainMenuActionClearErrors
            ]

    Login loginData -> do
      login loginData $ \case
        (Left rsp) -> handleError rsp $ \case
          ApiUserNotFound e -> [MainMenuAction . MainMenuActionLoginError $ "could not login as " <> e]
          _                 -> []

        (Right username) -> do
          pure $ dispatchMany
            [ ChangeCurrentUser $ UserLoggedIn username
            , MainMenuAction MainMenuActionClose
            ]

    Logout -> do
      logout $ \case
        (Left rsp) -> handleError rsp (const [])
        (Right ()) -> do
          pure $ dispatchMany
            [ ChangeCurrentUser UserLoggedOut
            , MainMenuAction MainMenuActionClose
            ]

    LoadTranslations locate -> do
      getTranslations (RT.GetTranslations locate) $ \case
        (Left rsp) -> handleError rsp (const [])
        (Right l10) -> do
          pure . dispatch $ ChangeTranslations l10

    _ -> pure ()

createChunkRange :: Maybe Range -> RT.ChunkRange
createChunkRange Nothing = RT.ChunkRange Nothing Nothing
createChunkRange (Just range) = RT.ChunkRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)

handleError :: (Int, String) -> (ApiError -> [GlobalAction]) -> IO [SomeStoreAction]
handleError (code, rsp) onApiError = case AE.eitherDecode $ cs rsp of
  Left err -> do
    consoleLogJSStringM "handleError: backend sent invalid response: " . cs $ unwords [show code, rsp, err]
        -- FIXME: use 'gracefulError' here.  (rename 'gracefulError' to 'assertContract'?)
    pure []
  Right apiError -> do
    consoleLogJSStringM "handleApiError" . cs $ show apiError
    pure . mconcat . fmap dispatch $ onApiError apiError


-- FIXME: return a single some-action, not a list?
dispatch :: GlobalAction -> [SomeStoreAction]
dispatch a = [someStoreAction @GlobalState a]

dispatchMany :: [GlobalAction] -> [SomeStoreAction]
dispatchMany = mconcat . fmap dispatch


getRange :: IO (Maybe Range)
getRange = (AE.decode . cs . unpack) <$> js_getRange

foreign import javascript unsafe
    "refine$getSelectionRange()"
    js_getRange :: IO JSString

foreign import javascript unsafe
  "window.getSelection().removeAllRanges();"
  js_removeAllRanges :: IO ()


-- * ugly hacks

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.
reactFluxWorkAroundForkIO :: IO () -> IO ()
reactFluxWorkAroundForkIO action = void . forkIO $ yield >> action

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.  Try to increase microseconds if you still experience race conditions.
reactFluxWorkAroundThreadDelay :: IO ()
reactFluxWorkAroundThreadDelay = threadDelay 10000
