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

import qualified Refine.Common.Types as C
import           Refine.Common.VDoc.Draft
import           Refine.Common.Rest (ApiError(..))
import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Store (setAllVerticalSpanBounds, documentStateUpdate)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Store (headerStateUpdate)
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Login.Store (loginStateUpdate)
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Store (mainMenuUpdate)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Rest
import qualified Refine.Frontend.Rest as Rest
import           Refine.Frontend.Screen.Store (screenStateUpdate)
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console
import           Refine.Frontend.Translation.Store (translationsUpdate)
import           Refine.Frontend.Types
import           Refine.Frontend.Util


instance StoreData GlobalState where
    type StoreAction GlobalState = GlobalAction
    transform = loop . \case
      CompositeAction as -> as
      a -> [a]
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
        loop (act : acts) st = do
          (st', acts') <- runStateT (transformGlobalState @Transform act st) []
          loop (acts <> acts') st'

type MonadTransform m = (Functor m, Applicative m, Monad m, MonadIO m, MonadState [GlobalAction] m)
type Transform = StateT [GlobalAction] IO

-- | FUTUREWORK: have more fine-grained constraints on 'm'.
transformGlobalState :: forall m. (HasCallStack, MonadTransform m) => GlobalAction -> GlobalState -> m GlobalState
transformGlobalState = transf
  where
    transf :: GlobalAction -> GlobalState -> m GlobalState
    transf (ResetState st) _ = pure st  -- for testing only!
    transf act st = do
      consoleLogGlobalStateBefore weAreInDevMode act st

      let st' = pureTransform act st

      -- ajax
      liftIO $ emitBackendCallsFor act st

      -- other effects
      case act of
        ContributionAction RequestSetAllVerticalSpanBounds -> do
          mapM_ (dispatchAndExec . ContributionAction) =<< setAllVerticalSpanBounds (st ^. gsDocumentState)

        ContributionAction RequestSetRange -> do
          mRangeEvent <- getRangeAction $ getDocumentState st
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

        ContributionAction ShowCommentEditor                       -> scrollToCurrentSelection (st ^. gsContributionState)
        DocumentAction (DocumentSave (FormBegin EditIsNotInitial)) -> scrollToCurrentSelection (st ^. gsContributionState)
        LoadCompositeVDoc (AfterAjax _)                            -> liftIO js_scrollToPageTop  -- FIXME: #416.
        HeaderAction ScrollToPageTop                               -> liftIO js_scrollToPageTop
        HeaderAction (ScrollToBlockKey (C.BlockKey k))             -> liftIO . js_scrollToBlockKey $ cs k

        LoginGuardStash actions -> do
          case st ^. gsLoginState . lsCurrentUser of
            UserLoggedOut  -> dispatchAndExec . MainMenuAction . MainMenuActionOpen . MainMenuLogin $ MainMenuSubTabLogin
            UserLoggedIn _ -> dispatchAndExec `mapM_` actions

        LoginGuardPop -> do
          case st ^. gsLoginState . lsCurrentUser of
            UserLoggedOut  -> error "LoginGuardPop before logged in!"
            UserLoggedIn _ -> dispatchAndExec `mapM_` (st ^. gsDispatchAfterLogin)

        ShowNotImplementedYet -> do
            liftIO $ windowAlertST "not implemented yet."

        _ -> pure ()

      consoleLogGlobalStateAfter weAreInDevMode (st' /= st) st'
      pure st'

    pureTransform :: GlobalAction -> GlobalState -> GlobalState
    pureTransform act st = st'
      where st' = st
              & gsEditID              %~ editIDUpdate act
              & gsContributionState   %~ contributionStateUpdate act
              & gsHeaderState         %~ headerStateUpdate act
              & gsDocumentState       %~ documentStateUpdate act st st'
              & gsScreenState         %~ maybe id screenStateUpdate (act ^? _ScreenAction)
              & gsLoginState          %~ loginStateUpdate act
              & gsDispatchAfterLogin  %~ dispatchAfterLoginUpdate act (st ^. gsLoginState)
              & gsMainMenuState       %~ mainMenuUpdate act (isJust $ st ^. gsEditID)
              & gsToolbarSticky       %~ toolbarStickyUpdate act
              & gsTranslations        %~ translationsUpdate act
              & gsDevState            %~ devStateUpdate act
              & gsServerCache         %~ serverCacheUpdate act

editIDUpdate :: GlobalAction -> Maybe (ID C.Edit) -> Maybe (ID C.Edit)
editIDUpdate (LoadCompositeVDoc (AfterAjax cvd)) _ = Just $ cvd ^. C.compositeVDocThisEdit . C.editID
editIDUpdate _ st = st

serverCacheUpdate :: GlobalAction -> ServerCache -> ServerCache
serverCacheUpdate a c = case a of
  LoadVDoc (AfterAjax vdoc)
    -> c & scVDocs %~ M.insert (vdoc ^. C.vdocID) vdoc
  LoadCompositeVDoc (AfterAjax cvdoc)
    -> serverCacheUpdate (LoadVDoc . AfterAjax $ cvdoc ^. C.compositeVDoc)
     $ serverCacheUpdate (AddEdit $ cvdoc ^. C.compositeVDocThisEdit) c
  AddStatement _upd _cid (AfterAjax discussion)
    -> c & scDiscussions %~ M.insert (discussion ^. C.discussionID) discussion
  AddDiscussion discussion
    -> c & scDiscussions %~ M.insert (discussion ^. C.discussionID) discussion
  AddNote note
    -> c & scNotes %~ M.insert (note ^. C.noteID) note
  AddEdit edit
    -> c & scEdits %~ M.insert (edit ^. C.editID) edit
  SetCurrentUser (UserLoggedIn user)
    -> c & scUsers %~ M.insert (user ^. C.userID) user
  RefreshServerCache c'
    -> c' <> c
  _ -> c


consoleLogGlobalStateBefore :: HasCallStack => forall m. MonadTransform m => Bool -> GlobalAction -> GlobalState -> m ()
consoleLogGlobalStateBefore False _ _ = pure ()
consoleLogGlobalStateBefore True act _st = do
  liftIO $ consoleLogJSStringM "" "\n"
  consoleLogGlobalAction act

consoleLogGlobalStateAfter :: HasCallStack => forall m. MonadTransform m => Bool -> Bool -> GlobalState -> m ()
consoleLogGlobalStateAfter False = \_ _ -> pure ()
consoleLogGlobalStateAfter True  = consoleLogGlobalState

consoleLogGlobalState :: HasCallStack => forall m. MonadTransform m => Bool {- changed -} -> GlobalState -> m ()
consoleLogGlobalState False _ = do
  consoleLogJSONM "New state: " (String "[UNCHANGED]" :: Value)
consoleLogGlobalState True st = liftIO $ do
  consoleLogJSONM "New state: " st
  traceEditorState `mapM_` (st ^? gsDocumentState . documentStateVal)
  traceContentInEditorState `mapM_` (st ^? gsDocumentState . documentStateVal)

consoleLogGlobalAction :: HasCallStack => forall m. MonadTransform m => GlobalAction -> m ()
consoleLogGlobalAction act = do
  let consolewidth = 80
      shown = show act
  if length shown <= consolewidth
    then do
      consoleLogJSStringM "Action: " (cs shown)
    else do
      consoleLogJSStringM "Action: " (cs $ take consolewidth shown)
      consoleLogJSONM "Action: " act


-- * pure updates

toolbarStickyUpdate :: HasCallStack => GlobalAction -> Bool -> Bool
toolbarStickyUpdate act st = case act of
  ToolbarStickyStateChange st' -> st'
  _                            -> st


-- | Only touches the 'DevState' if it is 'Just'.  In production, 'gsDevState' should always be
-- 'Nothing'.  Use 'weAreInDevMode' to decide whether you want to initialize it, or, when writing
-- test cases, initialize it unconditionally (See `test/Refine/Frontend/Contribution/MarkSpec.hs`).
devStateUpdate :: HasCallStack => GlobalAction -> Maybe DevState -> Maybe DevState
devStateUpdate _ Nothing = Nothing
devStateUpdate act (Just devstate) = Just $ upd act devstate
  where
    upd a (DevState as) = DevState $ a : as

dispatchAfterLoginUpdate :: HasCallStack => GlobalAction -> LoginState -> [GlobalAction] -> [GlobalAction]
dispatchAfterLoginUpdate (LoginGuardStash as) (LoginState UserLoggedOut) = (<> as)
dispatchAfterLoginUpdate _ _ = id


-- * ajax

emitBackendCallsFor :: HasCallStack => GlobalAction -> GlobalState -> IO ()
emitBackendCallsFor act st = case act of

    -- documents

    LoadCompositeVDoc (BeforeAjax auid) -> do
        getVDoc auid $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right loadedVDoc) -> dispatchM . CompositeAction $
                                   [ LoadCompositeVDoc $ AfterAjax loadedVDoc
                                   , ContributionAction RequestSetAllVerticalSpanBounds
                                   ]

    -- groups

    MainMenuAction (MainMenuActionOpen (MainMenuGroups BeforeAjax{})) -> do
        getGroups $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right groups) -> dispatchM . CompositeAction $
              [ RefreshServerCache . ServerCache mempty mempty mempty mempty mempty
                $ M.fromList [(g ^. C.groupID, g) | g <- groups]
              , MainMenuAction . MainMenuActionOpen . MainMenuGroups . AfterAjax $ (^. C.groupID) <$> groups
              ]

    MainMenuAction (MainMenuActionOpen (MainMenuCreateOrUpdateGroup mid (FormComplete cg))) -> do
        maybe createGroup updateGroup mid cg $ \case
            Left rsp -> ajaxFail rsp Nothing
            Right rsp -> dispatchM . CompositeAction $
              [ RefreshServerCache . ServerCache mempty mempty mempty mempty mempty
                $ M.fromList [(rsp ^. C.groupID, rsp)]
              , MainMenuAction . MainMenuActionOpen . MainMenuGroup $ rsp ^. C.groupID
              ]

    MainMenuAction (MainMenuActionOpen (MainMenuCreateProcess (FormComplete cg))) -> do
        createVDoc cg $ \case
            Left rsp -> ajaxFail rsp Nothing
            Right loadedVDoc -> dispatchM . LoadCompositeVDoc $ AfterAjax loadedVDoc

    MainMenuAction (MainMenuActionOpen (MainMenuUpdateProcess vid (FormComplete cg))) -> do
        updateVDoc vid cg $ \case
            Left rsp -> ajaxFail rsp Nothing
            Right vdoc -> dispatchM . LoadVDoc $ AfterAjax vdoc


    -- contributions

    AddStatement upd sid (BeforeAjax statement) -> do
        (if upd then updateStatement else addStatement) sid statement $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right discussion) -> dispatchM . AddStatement upd sid $ AfterAjax discussion

    ContributionAction (SubmitComment (CommentInfo text kind)) -> do
      let headEdit = fromMaybe (error "emitBackendCallsFor.SubmitComment")
                   $ st ^? gsVDoc . _Just . C.compositeVDoc . C.vdocHeadEdit
          range    = fromMaybe (minimumRange (fromMaybe (error "perhaps we should make documentStateContent a proper lens?") $
                                              st ^? to getDocumentState . documentStateContent))
                   $ st ^? gsCurrentSelection . _Just . C.selectionRange
          handle a = dispatchM . CompositeAction $
                       [ a
                       , ContributionAction RequestSetAllVerticalSpanBounds
                       , reloadCompositeVDoc st
                       , DocumentAction UpdateDocumentStateView
                       ]

      case kind of
        CommentKindDiscussion ->
          addDiscussion headEdit (C.CreateDiscussion text True range) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right discussion) -> handle $ AddDiscussion discussion
        CommentKindNote ->
          addNote headEdit (C.CreateNote text True range) $ \case
            (Left rsp) -> ajaxFail rsp Nothing
            (Right note) -> handle $ AddNote note

    DocumentAction (DocumentSave (FormBegin EditIsInitial))
      -> do
        let DocumentStateEdit editorState _ (Just baseEdit) = st ^. gsDocumentState
            cedit = C.CreateEdit
                  { C._createEditDesc        = "initial content"
                  , C._createEditVDocVersion = getCurrentRawContent editorState
                  , C._createEditKind        = C.Initial
                  }

        addEdit baseEdit cedit $ \case  -- TUNING: this is less than ideal.
          Left rsp   -> ajaxFail rsp Nothing
          Right edit -> do
            mergeEdit (edit ^. C.editID) $ \case
              Left rsp   -> ajaxFail rsp Nothing
              Right ()   -> dispatchM . reloadCompositeVDoc' $ edit ^. C.editVDoc
            dispatchM $ AddEdit edit

    DocumentAction (DocumentSave (FormComplete info))
      | DocumentStateEdit editorState _ baseEdit_ <- st ^. gsDocumentState
      -> do
        let baseEdit :: C.ID C.Edit
            (baseEdit:_) = catMaybes [baseEdit_, st ^? gsVDoc . _Just . C.compositeVDocThisEditID]

            cedit = C.CreateEdit
                  { C._createEditDesc        = info ^. editInfoDesc
                  , C._createEditVDocVersion = getCurrentRawContent editorState
                  , C._createEditKind        = info ^. editInfoKind
                  }

        addEdit baseEdit cedit $ \case
          Left rsp   -> ajaxFail rsp Nothing
          Right edit -> dispatchM . CompositeAction $
            [ AddEdit edit
            , ContributionAction RequestSetAllVerticalSpanBounds
            , reloadCompositeVDoc' (edit ^. C.editVDoc)
            , DocumentAction UpdateDocumentStateView
            ]


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
          dispatchM . CompositeAction $
            [ MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)
            , MainMenuAction MainMenuActionClearErrors
            ]

    Login loginData -> do
      login loginData $ \case
        (Left rsp) -> ajaxFail rsp . Just $ \case
          ApiUserNotFound e -> [MainMenuAction $ MainMenuActionLoginError e]
          _                 -> []

        (Right user) -> do
          dispatchM . CompositeAction $
            [ SetCurrentUser $ UserLoggedIn user
            , MainMenuAction MainMenuActionClose
            , LoginGuardPop
            ]

    Logout -> do
      logout $ \case
        (Left rsp) -> ajaxFail rsp Nothing
        (Right ()) -> do
          dispatchM . CompositeAction $
            [ SetCurrentUser UserLoggedOut
            , MainMenuAction MainMenuActionClose
            ]


    -- voting

    ContributionAction (ToggleVoteOnContribution eid vote) -> do
      sPutSimpleVoteOnEdit eid vote $ \case
          Left msg -> ajaxFail msg Nothing
          Right () -> dispatchM $ reloadCompositeVDoc st


    -- cache

    PopulateCache k -> case k of
      CacheKeyVDoc i -> Rest.getVDocSimple i $ \case
          Left msg -> ajaxFail msg Nothing
          Right val -> dispatchM . RefreshServerCache $ ServerCache (M.singleton i val) mempty mempty mempty mempty mempty
      CacheKeyEdit i -> Rest.getEdit i $ \case
          Left msg -> ajaxFail msg Nothing
          Right val -> dispatchM . RefreshServerCache $ ServerCache mempty (M.singleton i val) mempty mempty mempty mempty
      CacheKeyNote i -> getNote i $ \case
          Left msg -> ajaxFail msg Nothing
          Right val -> dispatchM . RefreshServerCache $ ServerCache mempty mempty (M.singleton i val) mempty mempty mempty
      CacheKeyDiscussion i -> Rest.getDiscussion i $ \case
          Left msg -> ajaxFail msg Nothing
          Right val -> dispatchM . RefreshServerCache $ ServerCache mempty mempty mempty (M.singleton i val) mempty mempty
      CacheKeyUser i -> getUser i $ \case
          Left msg -> ajaxFail msg Nothing
          Right val -> dispatchM . RefreshServerCache $ ServerCache mempty mempty mempty mempty (M.singleton i val) mempty
      CacheKeyGroup i -> getGroup i $ \case
          Left msg -> ajaxFail msg Nothing
          Right val -> dispatchM . RefreshServerCache $ ServerCache mempty mempty mempty mempty mempty (M.singleton i val)


    -- default

    _ -> pure ()


-- | TUNING: the calls to this action are usually outrageously
-- expensive, but we don't have a generic way to incrementally update
-- the composite vdoc here.  if we got rid of the NFData constraint on
-- actions, we could define @UpdateCVDoc :: (CompositeVDoc ->
-- CompositeVDoc) -> GlobalAction@.
reloadCompositeVDoc' :: HasCallStack => ID C.VDoc -> GlobalAction
reloadCompositeVDoc' = LoadCompositeVDoc . BeforeAjax

reloadCompositeVDoc :: HasCallStack => GlobalState -> GlobalAction
reloadCompositeVDoc = reloadCompositeVDoc'
  . fromMaybe (error "reloadCompositeVDoc")
  . (^? gsVDoc . _Just . C.compositeVDoc . C.vdocID)

ajaxFail :: HasCallStack => (Int, String) -> Maybe (ApiError -> [GlobalAction]) -> IO [SomeStoreAction]
ajaxFail (code, rsp) mOnApiError = case (eitherDecode $ cs rsp, mOnApiError) of
  (Right err, Just onApiError) -> mconcat <$> (dispatchM `mapM` onApiError err)
  (Right err, Nothing)         -> windowAlert ("Unexpected error from server: " <> show (code, err))     >> pure []
  (Left bad, _)                -> windowAlert ("Corrupted error from server: " <> show (code, rsp, bad)) >> pure []


-- * triggering actions

-- FIXME: return a single some-action, not a list?
dispatch :: HasCallStack => GlobalAction -> ViewEventHandler
dispatch a = [action @GlobalState a]

dispatchM :: HasCallStack => Monad m => GlobalAction -> m ViewEventHandler
dispatchM = pure . dispatch

reDispatchM :: HasCallStack => MonadState [GlobalAction] m => GlobalAction -> m ()
reDispatchM a = modify (<> [a])

dispatchAndExec :: HasCallStack => MonadIO m => GlobalAction -> m ()
dispatchAndExec a = liftIO . void . forkIO $ do
  () <- executeAction `mapM_` dispatch a
  pure ()


-- * ranges and selections

-- FIXME: move this section to somewhere in Document.* modules, together with the Range type.

-- | See also: 'Range' type.  Empty selection (start point == end point) counts as no selection, and
-- triggers a 'ClearRange' action to be emitted.  Only call this in `readOnly` mode.
--
-- IO is needed for (1) going via the selection state in the browser api (@getSelection (dstate
-- ^. documentStateVal)@ would be nicer, but draft does not store selections in readOnly mode.), and
-- for (2) for looking at the DOM for the position data.
getRangeAction :: (HasCallStack, MonadIO m) => DocumentState -> m (Maybe ContributionAction)
getRangeAction dstate = assert (has _DocumentStateView dstate) $ do
  esel :: Either String C.SelectionState <- runExceptT getDraftSelectionStateViaBrowser
  let rc :: C.RawContent
      Just rc = dstate ^? documentStateContent

  case esel of
    Left err -> do
      consoleLogJSONM "getRangeSelection: error" err
      pure $ Just ClearRange
    Right sel | rangeIsEmpty rc . C._selectionRange $ C.fromSelectionState rc sel -> do
      pure $ Just ClearRange
    Right sel -> Just . SetRange <$> do
      topOffset    <- liftIO js_getRangeTopOffset
      bottomOffset <- liftIO js_getRangeBottomOffset
      scrollOffset <- liftIO js_getScrollOffset
      let doctop = scrollOffset + if sel ^. C.unSelectionState . C.selectionIsBackward then topOffset else bottomOffset

      pure SelectionStateWithPx
        { _sstSelectionState = C.fromSelectionState rc sel
        , _sstDocTopOffset   = OffsetFromDocumentTop  doctop
        , _sstTopOffset      = OffsetFromViewportTop  topOffset
        , _sstBottomOffset   = OffsetFromViewportTop  bottomOffset
        , _sstScrollOffset   = ScrollOffsetOfViewport scrollOffset
        }

removeAllRanges :: HasCallStack => MonadIO m => m ()
removeAllRanges = liftIO js_removeAllRanges


-- * work-arounds for known bugs.

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.
reactFluxWorkAroundForkIO :: HasCallStack => IO () -> IO ()
reactFluxWorkAroundForkIO act = void . forkIO $ yield >> act

-- | See https://bitbucket.org/wuzzeb/react-flux/issues/28/triggering-re-render-after-store-update
-- for details and status.  Try to increase microseconds if you still experience race conditions.
reactFluxWorkAroundThreadDelay :: HasCallStack => Double -> IO ()
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
