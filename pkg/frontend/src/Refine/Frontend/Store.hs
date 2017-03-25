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
import           Control.Lens (_Just, (&), (^.), (^?), (^?!), (%~))
import           Control.Monad (void)
import           Data.Aeson (decode, eitherDecode)
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
    transform = transformGlobalState

-- | FUTUREWORK: have more fine-grained constraints on 'm'.
transformGlobalState :: m ~ IO => GlobalAction -> GlobalState -> m GlobalState
transformGlobalState = transf
  where
    pureTransform :: GlobalAction -> GlobalState -> GlobalState
    pureTransform action state = state
      & gsVDoc                       %~ vdocUpdate action
      & gsVDocList                   %~ vdocListUpdate action
      & gsContributionState          %~ maybe id contributionStateUpdate (action ^? _ContributionAction)
      & gsHeaderState                %~ headerStateUpdate action
      & gsDocumentState              %~ documentStateUpdate action (state ^? gsVDoc . _Just . C.compositeVDocVersion)
      & gsScreenState                %~ maybe id screenStateUpdate (action ^? _ScreenAction)
      & gsNotImplementedYetIsVisible %~ notImplementedYetIsVisibleUpdate action
      & gsLoginState                 %~ loginStateUpdate action
      & gsMainMenuState              %~ mainMenuUpdate action
      & gsToolbarSticky              %~ toolbarStickyUpdate action
      & gsTranslations               %~ fmap (translationsUpdate action)

    transf :: m ~ IO => GlobalAction -> GlobalState -> m GlobalState
    transf ClearState _ = pure emptyGlobalState  -- for testing only!
    transf action state = do
        consoleLogJSONM "Old state: " state
        consoleLogJSStringM "Action: " (cs $ show action)

        -- all updates to state are pure!
        let state' = pureTransform action state

        -- ajax
        emitBackendCallsFor action state

        -- other effects
        case action of
            TriggerUpdateSelection releasePositionOnPage toolbarStatus -> do
                mrange <- getRange
                dispatchM . ContributionAction $ UpdateSelection
                    (case mrange of
                        Nothing -> NothingSelectedButUpdateTriggered releasePositionOnPage
                        Just range -> RangeSelected range releasePositionOnPage)
                    toolbarStatus

            ContributionAction (ShowCommentEditor _) -> do
                js_removeAllRanges

            _ -> pure ()

        consoleLogJSONM "New state: " state'
        pure state'


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

    ContributionAction (ShowCommentEditor (Just range))
      -> vdoc & C.compositeVDocVersion %~ C.highlightRange (range ^. rangeStartPoint) (range ^. rangeEndPoint)
    ContributionAction HideCommentEditor
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
            (Right loadedVDocs) -> pure . dispatch $ LoadedDocumentList ((^. C.vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDoc) -> pure . dispatch $ OpenDocument loadedVDoc


    -- contributions

    ContributionAction (SubmitComment text category forRange) -> do
      -- here we need to distinguish which comment category we want to submit
      -- check the state and what the user selected there
      -- (FIXME: the new correct technical term for 'category' is 'kind'.)
      case category of
        Just Discussion ->
          addDiscussion (state ^?! gsVDoc . _Just . C.compositeVDocRepo . C.vdocHeadEdit)
                     (C.CreateDiscussion text True (createChunkRange forRange)) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right discussion) -> pure $ dispatch (AddDiscussion discussion)
        Just Note ->
          addNote (state ^?! gsVDoc . _Just . C.compositeVDocRepo . C.vdocHeadEdit)
                     (C.CreateNote text True (createChunkRange forRange)) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right note) -> pure $ dispatch (AddNote note)
        Nothing -> pure ()

    DocumentAction DocumentEditSave -> case state ^? gsDocumentState . documentStateEdit of
      Just estate@(editorStateToVDocVersion -> Right newvers) -> do
        let eid :: C.ID C.Edit
            eid = state ^?! gsVDoc . _Just . C.compositeVDocEditID

            cid :: C.Create C.Edit
            cid = C.CreateEdit
                  { C._createEditDesc  = "..."                          -- TODO: #233
                  , C._createEditRange = C.ChunkRange Nothing Nothing  -- FIXME: 'state ^. gsContributionState . csCurrentSelection'
                  , C._createEditVDoc  = C.downgradeVDocVersionWR newvers
                  , C._createEditKind  = estate ^. editorStateKind
                  , C._createEditMotiv = "..."                          -- TODO: #233
                  }

        addEdit eid cid $ \case
          Left rsp   -> handleError rsp (const [])
          Right edit -> pure $ dispatch (AddEdit edit)

      bad -> let msg = "DocumentAction DocumentEditSave: "
                    <> "not in editor state or content cannot be converted to html."
                    <> show bad
             in gracefulError msg $ pure ()


    -- i18n

    LoadTranslations locate -> do
      getTranslations (C.GetTranslations locate) $ \case
        (Left rsp) -> handleError rsp (const [])
        (Right l10) -> do
          pure . dispatch $ ChangeTranslations l10


    -- users

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


    -- testing & dev

    AddDemoDocument -> do
        createVDoc (C.CreateVDoc sampleTitle sampleAbstract sampleText) $ \case
            (Left rsp) -> handleError rsp (const [])
            (Right loadedVDoc) -> pure . dispatch $ OpenDocument loadedVDoc


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


-- * helpers

-- FIXME: return a single some-action, not a list?
dispatch :: GlobalAction -> [SomeStoreAction]
dispatch a = [someStoreAction @GlobalState a]

dispatchM :: GlobalAction -> IO ()
dispatchM = mapM_ executeAction . dispatch

dispatchMany :: [GlobalAction] -> [SomeStoreAction]
dispatchMany = mconcat . fmap dispatch

dispatchManyM :: [GlobalAction] -> IO ()
dispatchManyM = mapM_ executeAction . dispatchMany


getRange :: IO (Maybe Range)
getRange = decode . cs . unpack <$> js_getRange

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
