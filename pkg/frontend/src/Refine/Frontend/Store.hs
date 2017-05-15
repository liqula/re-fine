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
import           Control.Exception (assert)
import           Control.Lens (Lens', _Just, (&), (^.), (.~), (^?), (^?!), (%~), has)
import           Control.Monad ((<=<))
import           Control.Monad.IO.Class
import           Control.Monad.State.Class (MonadState, modify)
import           Control.Monad.Trans.State (StateT, runStateT)
import           Control.Monad (void, when)
import           Data.Aeson (eitherDecode, Value(String))
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.String.Conversions
import           React.Flux

import           Refine.Common.Types (CompositeVDoc(..))
import qualified Refine.Common.Types as C
import           Refine.Common.VDoc.Draft
import           Refine.Common.Rest (ApiError(..))
import           Refine.Common.Test.Samples
import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI (getSelection, traceContentInEditorState, traceEditorState)
import           Refine.Frontend.Document.Store (setMarkPositions, documentStateUpdate, editorStateToVDocVersion)
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
        -- straight-forwardly as @forkIO . dispatchM@.
        --
        -- (see also 'dispatchAndExec' below. change this only when switching to a future
        -- version of react-flux that has a monad-constraint-based interface.  then we'll have
        -- @MonadState GlobalState@ here and probably can get rid of the need for redispatch
        -- altogether, because it will be more easy to just apply a local state modification
        -- instead.  which raises the question whether we want to keep the separation between the
        -- pure state update and effects.)
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
    transf (ResetState s) _ = pure s  -- for testing only!
    transf action state = do
        consoleLogGlobalStateBefore weAreInDevMode action state

        let state' = pureTransform action state

        -- ajax
        liftIO $ emitBackendCallsFor action state

        -- other effects
        case action of
            DocumentAction (DocumentUpdate dstate@DocumentStateView{}) -> do
                (dispatchAndExec . ContributionAction <=< setMarkPositions) `mapM_` (dstate ^? documentStateContent)

                mRangeEvent <- getRangeAction (state ^. gsDocumentState) dstate
                case mRangeEvent of
                    Nothing -> pure ()
                    Just rangeEvent -> do
                        reDispatchM $ ContributionAction rangeEvent
                        -- TODO: call 'removeAllRanges' here and handle highlighting of the current
                        -- selection ourselves.  (we may want to only do that in read-only mode, or
                        -- draft may get confused and kill its own selection as well.)

                        when (state ^. gsHeaderState . hsToolbarExtensionStatus == CommentToolbarExtensionWithRange) $ do
                          -- (if the comment editor (or dialog) is started via the toolbar
                          -- extension, this is where it should be started.  assume that this can
                          -- only happen if rangeEvent is SetRange, not ClearRange.)
                          reDispatchM $ ContributionAction ShowCommentEditor

            ShowNotImplementedYet -> do
                liftIO $ windowAlertST "not implemented yet."

            _ -> pure ()

        consoleLogGlobalStateAfter weAreInDevMode (state' /= state) state'
        pure state'

    pureTransform :: GlobalAction -> GlobalState -> GlobalState
    pureTransform action state = state'
      where state' = state
              & gsVDoc                %~ vdocUpdate action
              & gsVDocList            %~ vdocListUpdate action
              & gsContributionState   %~ contributionStateUpdate action
              & gsHeaderState         %~ headerStateUpdate action
              & gsDocumentState       %~ documentStateUpdate action (state' ^. gsVDoc)
              & gsScreenState         %~ maybe id screenStateUpdate (action ^? _ScreenAction)
              & gsLoginState          %~ loginStateUpdate action
              & gsMainMenuState       %~ mainMenuUpdate action
              & gsToolbarSticky       %~ toolbarStickyUpdate action
              & gsTranslations        %~ translationsUpdate action
              & gsDevState            %~ devStateUpdate action


consoleLogGlobalStateBefore :: forall m. MonadTransform m => Bool -> GlobalAction -> GlobalState -> m ()
consoleLogGlobalStateBefore False _ _ = pure ()
consoleLogGlobalStateBefore True action state = liftIO $ do
  consoleLogJSStringM "" "\n"
  consoleLogJSONM "Old state: " state
  traceEditorState (state ^. gsDocumentState . documentStateVal)
  traceContentInEditorState (state ^. gsDocumentState . documentStateVal)
  consoleLogJSONM "Action: " action
  -- consoleLogJSStringM "Action: " (cs $ show action)

consoleLogGlobalStateAfter :: forall m. MonadTransform m => Bool -> Bool -> GlobalState -> m ()
consoleLogGlobalStateAfter False _ _ = pure ()
consoleLogGlobalStateAfter True False _ = do
  consoleLogJSONM "New state: " (String "[UNCHANGED]" :: Value)
consoleLogGlobalStateAfter True True state = liftIO $ do
  consoleLogJSONM "New state: " state
  traceEditorState (state ^. gsDocumentState . documentStateVal)
  traceContentInEditorState (state ^. gsDocumentState . documentStateVal)


-- * pure updates

vdocUpdate :: GlobalAction -> Maybe CompositeVDoc -> Maybe CompositeVDoc
vdocUpdate action Nothing = case action of
    OpenDocument newvdoc -> Just newvdoc
    _ -> Nothing

vdocUpdate action (Just vdoc) = Just $ case action of
    AddDiscussion discussion
      -> vdoc
          & C.compositeVDocDiscussions
              %~ M.insert (discussion ^. C.compositeDiscussion . C.discussionID) discussion

    AddNote note
      -> vdoc
          & C.compositeVDocNotes
              %~ M.insert (note ^. C.noteID) note

    AddEdit edit
      -> vdoc
          & C.compositeVDocEdits
              %~ M.insert (edit ^. C.editID) edit

    _ -> vdoc


vdocListUpdate :: GlobalAction -> Maybe [C.ID C.VDoc] -> Maybe [C.ID C.VDoc]
vdocListUpdate action state = case action of
    LoadedDocumentList list -> Just list
    _ -> state


toolbarStickyUpdate :: GlobalAction -> Bool -> Bool
toolbarStickyUpdate action state = case action of
  ToolbarStickyStateChange state' -> state'
  _                               -> state


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
emitBackendCallsFor action state = case action of

    -- documents

    LoadDocumentList -> do
        listVDocs $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDocs) -> dispatchM $ LoadedDocumentList ((^. C.vdocID) <$> loadedVDocs)
    LoadDocument auid -> do
        getVDoc auid $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDoc) -> dispatchM $ OpenDocument loadedVDoc


    -- contributions

    ContributionAction (SubmitComment text kind) -> do
      case kind of
        Just CommentKindDiscussion ->
          addDiscussion (state ^?! gsVDoc . _Just . C.compositeVDoc . C.vdocHeadEdit)
                     (C.CreateDiscussion text True (state ^. gsChunkRange)) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right discussion) -> dispatchM $ AddDiscussion discussion
        Just CommentKindNote ->
          addNote (state ^?! gsVDoc . _Just . C.compositeVDoc . C.vdocHeadEdit)
                     (C.CreateNote text True (state ^. gsChunkRange)) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right note) -> dispatchM $ AddNote note
        Nothing -> pure ()

    DocumentAction DocumentSave -> case state ^. gsDocumentState of
      dstate@(DocumentStateEdit _ kind) -> do
        let eid :: C.ID C.Edit
            eid = state ^?! gsVDoc . _Just . C.compositeVDocEditID

            cedit :: C.Create C.Edit
            cedit = C.CreateEdit
                  { C._createEditDesc  = "..."                          -- TODO: #233
                  , C._createEditRange = state ^. gsChunkRange
                  , C._createEditVDoc  = editorStateToVDocVersion (dstate ^. documentStateVal)
                  , C._createEditKind  = kind
                  , C._createEditMotiv = "..."                          -- TODO: #233
                  }

        addEdit eid cedit $ \case
          Left rsp   -> ajaxFail rsp Nothing
          Right edit -> dispatchM $ AddEdit edit

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
            [ MainMenuAction $ MainMenuActionOpen MainMenuLogin
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


    -- testing & dev

    AddDemoDocument -> do
        createVDoc (C.CreateVDoc sampleTitle sampleAbstract sampleVDocVersion) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDoc) -> dispatchM $ OpenDocument loadedVDoc


    -- default

    _ -> pure ()


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

gsChunkRange :: Lens' GlobalState C.ChunkRange
gsChunkRange f gs = outof <$> f (into gs)
  where
    into :: GlobalState -> C.ChunkRange
    into s = fromMaybe (C.ChunkRange Nothing Nothing)
               (s ^? gsContributionState . csCurrentRange . _Just . rangeSelectionState)

    outof :: C.ChunkRange -> GlobalState
    outof r = gs & gsContributionState . csCurrentRange . _Just . rangeSelectionState .~ r

-- | See also: 'Range' type.
--
-- Note that draft does not delete a selection if you single-click, but it creates an empty
-- selection, i.e. one where the start and the end point are identical.  In this case we emit a
-- 'ClearRange' action.
--
-- (This has to have IO because we look at the DOM for the position data.)
getRangeAction :: MonadIO m => DocumentState -> DocumentState -> m (Maybe ContributionAction)
getRangeAction beforeState afterState = assert (has _DocumentStateView beforeState) $ do
  let beforeSelection = getSelection (beforeState ^. documentStateVal)
      afterSelection  = getSelection (afterState ^. documentStateVal)
  case (beforeSelection == afterSelection, afterSelection) of
    (True,  _)
      -> pure Nothing
    (False, selectionIsEmpty (beforeState ^?! documentStateContent :: RawContent) -> True)
      -> pure $ Just ClearRange
    (False, sel)
      -> Just . SetRange <$> do
      topOffset    <- liftIO js_getRangeTopOffset
      bottomOffset <- liftIO js_getRangeBottomOffset
      scrollOffset <- liftIO js_getScrollOffset
      let doctop = scrollOffset + if sel ^. selectionIsBackward then topOffset else bottomOffset

      pure Range
        { _rangeSelectionState = selectionStateToChunkRange (beforeState ^?! documentStateContent) sel
        , _rangeDocTopOffset   = OffsetFromDocumentTop  doctop
        , _rangeTopOffset      = OffsetFromViewportTop  topOffset
        , _rangeBottomOffset   = OffsetFromViewportTop  bottomOffset
        , _rangeScrollOffset   = ScrollOffsetOfViewport scrollOffset
        }

foreign import javascript unsafe
  "getSelection().getRangeAt(0).startContainer.parentElement.getBoundingClientRect().top"
  js_getRangeTopOffset :: IO Int

foreign import javascript unsafe
  "getSelection().getRangeAt(0).endContainer.parentElement.getBoundingClientRect().bottom"
  js_getRangeBottomOffset :: IO Int

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
reactFluxWorkAroundThreadDelay :: Double -> IO ()
reactFluxWorkAroundThreadDelay seconds = threadDelay . round $ seconds * 1000 * 1000
