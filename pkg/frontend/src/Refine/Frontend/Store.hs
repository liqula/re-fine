{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Frontend.Prelude

import           Control.Concurrent (forkIO, yield, threadDelay)
import qualified Data.Map.Strict as M

import           Refine.Common.Types (CompositeVDoc(..))
import qualified Refine.Common.Types as C
import           Refine.Common.VDoc.Draft
import           Refine.Common.Rest (ApiError(..))
import           Refine.Common.Test.Samples
import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Store (setAllVertialSpanBounds, documentStateUpdate, editorStateToVDocVersion)
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
import           Refine.Frontend.Translation.Store (translationsUpdate)
import           Refine.Frontend.Types
import           Refine.Frontend.Util


instance StoreData GlobalState where
    type StoreAction GlobalState = GlobalAction
    transform = loop . (:[])
      where
        -- FUTUREWORK: we don't need this loop trick, we can implement reDispatch much more
        -- straight-forwardly as @forkIO . dispatchM@.  EXCEPT: if we process action A, then throw
        -- action B concurrently outside of processing action A, then throw action C from inside of
        -- processing A; which of B and C wins the race for the next lock?  needs more thinking!
        --
        -- (see also 'dispatchAndExec' below. change this only when switching to a future
        -- version of react-flux that has a monad-constraint-based interface.  then we'll have
        -- @MonadState GlobalState@ here and probably can get rid of the need for redispatch
        -- altogether, because it will be more easy to just apply a local state modification
        -- instead.  which raises the question whether we want to keep the separation between the
        -- pure state update and effects.)
        loop :: [GlobalAction] -> GlobalState -> IO GlobalState
        loop [] st = pure st
        loop (action : actions) st = do
          (st', actions') <- runStateT (transformGlobalState @Transform action st) []
          loop (actions <> actions') st'

type MonadTransform m = (Functor m, Applicative m, Monad m, MonadIO m, MonadState [GlobalAction] m)
type Transform = StateT [GlobalAction] IO

-- | FUTUREWORK: have more fine-grained constraints on 'm'.
transformGlobalState :: forall m. MonadTransform m => GlobalAction -> GlobalState -> m GlobalState
transformGlobalState = transf
  where
    transf :: GlobalAction -> GlobalState -> m GlobalState
    transf (ResetState st) _ = pure st  -- for testing only!
    transf action st = do
      consoleLogGlobalStateBefore weAreInDevMode action st

      let st' = pureTransform action st

      -- ajax
      liftIO $ emitBackendCallsFor action st

      -- other effects
      case action of
        ContributionAction RequestSetAllVertialSpanBounds -> do
          dispatchAndExec . ContributionAction =<< setAllVertialSpanBounds (st ^. gsDocumentState)

        ContributionAction RequestSetRange -> do
          mRangeEvent <- getRangeAction (st ^. gsDocumentState)
          case mRangeEvent of
            Nothing -> pure ()
            Just rangeEvent -> do
              reDispatchM $ ContributionAction rangeEvent
              when (st ^. gsHeaderState . hsToolbarExtensionStatus == CommentToolbarExtensionWithRange) $ do
                -- (if the comment editor (or dialog) is started via the toolbar
                -- extension, this is where it should be started.  assume that this can
                -- only happen if rangeEvent is SetRange, not ClearRange.)
                reDispatchM $ ContributionAction ShowCommentEditor

        ContributionAction (SetRange _) -> removeAllRanges
        ContributionAction ClearRange   -> removeAllRanges

        ContributionAction ShowCommentEditor            -> scrollToCurrentSelection (st ^. gsContributionState)
        DocumentAction RequestDocumentSave              -> scrollToCurrentSelection (st ^. gsContributionState)

        HeaderAction ScrollToPageTop -> liftIO js_scrollToPageTop

        ShowNotImplementedYet -> do
            liftIO $ windowAlertST "not implemented yet."

        _ -> pure ()

      consoleLogGlobalStateAfter weAreInDevMode (st' /= st) st'
      pure st'

    pureTransform :: GlobalAction -> GlobalState -> GlobalState
    pureTransform action st = st'
      where st' = st
              & gsVDoc                %~ vdocUpdate action
              & gsVDocList            %~ vdocListUpdate action
              & gsContributionState   %~ contributionStateUpdate action
              & gsHeaderState         %~ headerStateUpdate action
              & gsDocumentState       %~ documentStateUpdate action st'
              & gsScreenState         %~ maybe id screenStateUpdate (action ^? _ScreenAction)
              & gsLoginState          %~ loginStateUpdate action
              & gsMainMenuState       %~ mainMenuUpdate action
              & gsToolbarSticky       %~ toolbarStickyUpdate action
              & gsTranslations        %~ translationsUpdate action
              & gsDevState            %~ devStateUpdate action

consoleLogGlobalStateBefore :: forall m. MonadTransform m => Bool -> GlobalAction -> GlobalState -> m ()
consoleLogGlobalStateBefore False _ _ = pure ()
consoleLogGlobalStateBefore True action _st = liftIO $ do
  consoleLogJSStringM "" "\n"
  -- (do not show old state initially; it should still be in the logs from the last call to
  -- 'transformGlobalState' (except for with the first action, but we usually do not debug that).
  -- consoleLogJSONM "Old state: " st
  -- traceEditorState (st ^. gsDocumentState . documentStateVal)
  -- traceContentInEditorState (st ^. gsDocumentState . documentStateVal)
  consoleLogJSONM "Action: " action
  -- consoleLogJSStringM "Action: " (cs $ show action)

consoleLogGlobalStateAfter :: forall m. MonadTransform m => Bool -> Bool -> GlobalState -> m ()
consoleLogGlobalStateAfter False _ _ = pure ()
consoleLogGlobalStateAfter True False _ = do
  consoleLogJSONM "New state: " (String "[UNCHANGED]" :: Value)
consoleLogGlobalStateAfter True True st = liftIO $ do
  consoleLogJSONM "New state: " st
  traceEditorState (st ^. gsDocumentState . documentStateVal)
  traceContentInEditorState (st ^. gsDocumentState . documentStateVal)


-- * pure updates

vdocUpdate :: GlobalAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action Nothing = case action of
    OpenDocument newvdoc -> Just newvdoc
    _ -> Nothing

vdocUpdate action (Just vdoc) = Just $ case action of
    AddDiscussion discussion
      -> vdoc
          & C.compositeVDocApplicableDiscussions
              %~ M.insert (discussion ^. C.compositeDiscussion . C.discussionID) discussion

    AddNote note
      -> vdoc
          & C.compositeVDocApplicableNotes
              %~ M.insert (note ^. C.noteID) note

    AddEdit edit
      -> vdoc
          & C.compositeVDocApplicableEdits
              %~ M.insert (edit ^. C.editID) edit

    _ -> vdoc


vdocListUpdate :: GlobalAction -> Maybe [C.ID C.VDoc] -> Maybe [C.ID C.VDoc]
vdocListUpdate (RegisterDocumentList vdocs) _  = Just vdocs
vdocListUpdate _                            st = st


toolbarStickyUpdate :: GlobalAction -> Bool -> Bool
toolbarStickyUpdate action st = case action of
  ToolbarStickyStateChange st' -> st'
  _                            -> st


-- | Only touches the 'DevState' if it is 'Just'.  In production, 'gsDevState' should always be
-- 'Nothing'.  Use 'weAreInDevMode' to decide whether you want to initialize it, or, when writing
-- test cases, initialize it unconditionally (See `test/Refine/Frontend/Contribution/MarkSpec.hs`).
devStateUpdate :: GlobalAction -> Maybe DevState -> Maybe DevState
devStateUpdate _ Nothing = Nothing
devStateUpdate action (Just devstate) = Just $ upd action devstate
  where
    upd a (DevState as) = DevState $ a : as


-- * ajax

emitBackendCallsFor :: GlobalAction -> GlobalState -> IO ()
emitBackendCallsFor action st = case action of

    -- documents

    LoadDocumentList -> do
        listVDocs $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDocs) -> dispatchM $ RegisterDocumentList ((^. C.vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDoc) -> dispatchM $ OpenDocument loadedVDoc


    -- contributions

    ContributionAction (SubmitComment text kind) -> do
      case kind of
        Just CommentKindDiscussion ->
          addDiscussion (st ^?! gsVDoc . _Just . C.compositeVDoc . C.vdocHeadEdit)
                     (C.CreateDiscussion text True (st ^. gsCurrentSelection . C.selectionRange)) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right discussion) -> dispatchManyM [ AddDiscussion discussion
                                                , ContributionAction RequestSetAllVertialSpanBounds
                                                , reloadCompositeVDoc st
                                                ]
        Just CommentKindNote ->
          addNote (st ^?! gsVDoc . _Just . C.compositeVDoc . C.vdocHeadEdit)
                     (C.CreateNote text True (st ^. gsCurrentSelection . C.selectionRange)) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right note) -> dispatchManyM [ AddNote note
                                          , ContributionAction RequestSetAllVertialSpanBounds
                                          , reloadCompositeVDoc st
                                          ]
        Nothing -> pure ()

    DocumentAction (DocumentSave desc) -> case st ^. gsDocumentState of
      dstate@(DocumentStateEdit _ kind) -> do
        let eid :: C.ID C.Edit
            eid = st ^?! gsVDoc . _Just . C.compositeVDocThisEditID

            cedit :: C.Create C.Edit
            cedit = C.CreateEdit
                  { C._createEditDesc  = desc
                  , C._createEditVDoc  = editorStateToVDocVersion (dstate ^. documentStateVal)
                  , C._createEditKind  = kind
                  }

        addEdit eid cedit $ \case
          Left rsp   -> ajaxFail rsp Nothing
          Right edit -> dispatchManyM [ AddEdit edit
                                      , ContributionAction RequestSetAllVertialSpanBounds
                                      , reloadCompositeVDoc st
                                      ]

      bad -> let msg = "DocumentAction DocumentEditSave: "
                    <> "not in editor state or content cannot be converted to html."
                    <> show bad
             in gracefulError msg $ pure ()


    -- i18n

    LoadTranslations locate -> do
      getTranslations (C.GetTranslations locate) $ \case
        (Left rsp) -> ajaxFail rsp Nothing
        (Right l10) -> do
          dispatchM $ ChangeTranslations l10


    -- users

    CreateUser createUserData -> do
      createUser createUserData $ \case
        (Left rsp) -> ajaxFail rsp . Just $ \case
          ApiUserCreationError u -> [MainMenuAction $ MainMenuActionRegistrationError u]
          _                      -> []

        (Right _user) -> do
          dispatchManyM
            [ MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)
            , MainMenuAction MainMenuActionClearErrors
            ]

    Login loginData -> do
      login loginData $ \case
        (Left rsp) -> ajaxFail rsp . Just $ \case
          ApiUserNotFound e -> [MainMenuAction $ MainMenuActionLoginError e]
          _                 -> []

        (Right username) -> do
          dispatchManyM
            [ ChangeCurrentUser $ UserLoggedIn username
            , MainMenuAction MainMenuActionClose
            ]

    Logout -> do
      logout $ \case
        (Left rsp) -> ajaxFail rsp Nothing
        (Right ()) -> do
          dispatchManyM
            [ ChangeCurrentUser UserLoggedOut
            , MainMenuAction MainMenuActionClose
            ]


    -- voting

    ContributionAction (ToggleVoteOnContribution eid vote) -> do
      case st ^. gsLoginState . lsCurrentUser of
        UserLoggedOut       -> pure ()
        UserLoggedIn _uname -> sPutSimpleVoteOnEdit eid vote $ \case
          Left msg -> ajaxFail msg Nothing
          Right () -> dispatchM $ reloadCompositeVDoc st


    -- testing & dev

    AddDemoDocument -> do
        createVDoc (C.CreateVDoc sampleTitle sampleAbstract sampleVDocVersion) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDoc) -> dispatchM $ OpenDocument loadedVDoc


    -- default

    _ -> pure ()


-- | TUNING: the calls to this action are usually outrageously
-- expensive, but we don't have a generic way to incrementally update
-- the composite vdoc here.  if we got rid of the NFData constraint on
-- actions, we could define @UpdateCVDoc :: (CompositeVDoc ->
-- CompositeVDoc) -> GlobalAction@.
reloadCompositeVDoc :: GlobalState -> GlobalAction
reloadCompositeVDoc st = LoadDocument (st ^?! gsVDoc . _Just . C.compositeVDoc . C.vdocID)

ajaxFail :: (Int, String) -> Maybe (ApiError -> [GlobalAction]) -> IO [SomeStoreAction]
ajaxFail (code, rsp) mOnApiError = case (eitherDecode $ cs rsp, mOnApiError) of
  (Right err, Just onApiError) -> dispatchManyM (onApiError err)
  (Right err, Nothing)         -> windowAlert ("Unexpected error from server: " <> show (code, err))     >> pure []
  (Left bad, _)                -> windowAlert ("Corrupted error from server: " <> show (code, rsp, bad)) >> pure []


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
dispatchAndExec a = liftIO . void . forkIO $ do
  () <- executeAction `mapM_` dispatch a
  pure ()

dispatchAndExecMany :: MonadIO m => [GlobalAction] -> m ()
dispatchAndExecMany as = liftIO . void . forkIO $ do
  () <- executeAction `mapM_` dispatchMany as
  pure ()


-- * ranges and selections

-- FIXME: move this section to somewhere in Document.* modules, together with the Range type.

-- | See also: 'Range' type.  Empty selection (start point == end point) counts as no selection, and
-- triggers a 'ClearRange' action to be emitted.  Only call this in `readOnly` mode.
--
-- IO is needed for (1) going via the selection state in the browser api (@getSelection (dstate
-- ^. documentStateVal)@ would be nicer, but draft does not store selections in readOnly mode.), and
-- for (2) for looking at the DOM for the position data.
getRangeAction :: MonadIO m => DocumentState -> m (Maybe ContributionAction)
getRangeAction dstate = assert (has _DocumentStateView dstate) $ do
  esel :: Either String C.SelectionState <- runExceptT getDraftSelectionStateViaBrowser
  case esel of
    Left err -> do
      consoleLogJSONM "getRangeSelection: error" err
      pure $ Just ClearRange
    Right sel | rangeIsEmpty (dstate ^?! documentStateContent)
              . C._selectionRange
              $ C.fromSelectionState (dstate ^?! documentStateContent) sel -> do
      pure $ Just ClearRange
    Right sel -> Just . SetRange <$> do
      topOffset    <- liftIO js_getRangeTopOffset
      bottomOffset <- liftIO js_getRangeBottomOffset
      scrollOffset <- liftIO js_getScrollOffset
      let doctop = scrollOffset + if sel ^. C.unSelectionState . C.selectionIsBackward then topOffset else bottomOffset

      pure SelectionStateWithPx
        { _sstSelectionState = C.fromSelectionState (dstate ^?! documentStateContent) sel
        , _sstDocTopOffset   = OffsetFromDocumentTop  doctop
        , _sstTopOffset      = OffsetFromViewportTop  topOffset
        , _sstBottomOffset   = OffsetFromViewportTop  bottomOffset
        , _sstScrollOffset   = ScrollOffsetOfViewport scrollOffset
        }

removeAllRanges :: MonadIO m => m ()
removeAllRanges = liftIO js_removeAllRanges


-- * work-arounds for known bugs.

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.
reactFluxWorkAroundForkIO :: IO () -> IO ()
reactFluxWorkAroundForkIO action = void . forkIO $ yield >> action

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.  Try to increase microseconds if you still experience race conditions.
reactFluxWorkAroundThreadDelay :: Double -> IO ()
reactFluxWorkAroundThreadDelay seconds = threadDelay . round $ seconds * 1000 * 1000


-- * foreign

#ifdef __GHCJS__

foreign import javascript safe
  "getSelection().getRangeAt(0).startContainer.parentElement.getBoundingClientRect().top"
  js_getRangeTopOffset :: IO Int

foreign import javascript safe
  "getSelection().getRangeAt(0).endContainer.parentElement.getBoundingClientRect().bottom"
  js_getRangeBottomOffset :: IO Int

foreign import javascript safe
  "window.getSelection().removeAllRanges()"
  js_removeAllRanges :: IO ()

#else

{-# ANN js_getRangeTopOffset ("HLint: ignore Use camelCase" :: String) #-}
js_getRangeTopOffset :: IO Int
js_getRangeTopOffset = error "javascript FFI not available in GHC"

{-# ANN js_getRangeBottomOffset ("HLint: ignore Use camelCase" :: String) #-}
js_getRangeBottomOffset :: IO Int
js_getRangeBottomOffset = error "javascript FFI not available in GHC"

{-# ANN js_removeAllRanges ("HLint: ignore Use camelCase" :: String) #-}
js_removeAllRanges :: IO ()
js_removeAllRanges = error "javascript FFI not available in GHC"

#endif
