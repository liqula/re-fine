{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Store where
#include "import_frontend.hs"

import           Control.Concurrent
import           GHCJS.Foreign.Callback (Callback, asyncCallback1)

import           Refine.Common.Types hiding (CreateUser, Login)
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Access ()
import           Refine.Frontend.Contribution.Store (contributionStateUpdate)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Store (setAllVerticalSpanBounds, documentStateUpdate)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Store (headerStateUpdate)
import           Refine.Frontend.Header.Types
import           Refine.Frontend.MainMenu.Store (mainMenuUpdate)
import           Refine.Frontend.MainMenu.Types
import qualified Refine.Frontend.Route as Route
import           Refine.Frontend.Screen.Store (screenStateUpdate)
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console
import           Refine.Frontend.Translation.Store (translationsUpdate)
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import           Refine.Frontend.WebSocket


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

type MonadTransform m = (Applicative m, MonadIO m, MonadState [GlobalAction] m)
type Transform = StateT [GlobalAction] IO

-- | FUTUREWORK: have more fine-grained constraints on 'm'.
transformGlobalState :: forall m. (HasCallStack, MonadTransform m) => GlobalAction -> GlobalState -> m GlobalState
transformGlobalState = transf
  where
    transf :: GlobalAction -> GlobalState -> m GlobalState
    transf (ResetState st) _ = pure st  -- for testing only!
    transf act st = do
      consoleLogGlobalStateBefore weAreInDevMode act st

      st' <- pureTransform act st

      -- ajax
      liftIO $ emitBackendCallsFor act st

      -- scrolling
      case act of
        ContributionAction ShowCommentEditor                       -> scrollToCurrentSelection (st ^. gsContributionState)
        DocumentAction (DocumentSave (FormBegin EditIsNotInitial)) -> scrollToCurrentSelection (st ^. gsContributionState)
        HeaderAction (ScrollToBlockKey (Common.BlockKey k))        -> liftIO . js_scrollToBlockKey $ cs k
        HeaderAction ScrollToPageTop                               -> liftIO js_scrollToPageTop
        _ | not . null $ routesFromAction act                      -> liftIO js_scrollToPageTop
        _ -> pure ()

      -- routing
      Route.changeRoute `mapM_` (take 1 . reverse $ routesFromAction act)

      -- other effects
      case act of
        ContributionAction RequestSetAllVerticalSpanBounds -> do
          mapM_ (dispatchAndExec . ContributionAction) =<< setAllVerticalSpanBounds (st ^. gsDocumentState)

        ContributionAction RequestSetRange -> do
          mRangeEvent <- getRangeAction $ gsRawContent st
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

        ShowNotImplementedYet -> do
            liftIO $ windowAlertST "not implemented yet."

        _ -> pure ()

      consoleLogGlobalStateAfter weAreInDevMode (st' /= st) st'
      pure st'

    pureTransform :: GlobalAction -> GlobalState -> m GlobalState
    pureTransform act st = case runExcept $ fm st of
      Right b -> pure b
      Left () -> do
        liftIO flushCacheMisses
        liftIO $ threadDelay 152000   -- FIXME: #425
        dispatchAndExec act
        pure st
     where
      fm :: GlobalState -> CacheLookupT GlobalState
      fm =    gsServerCache         (pure . serverCacheUpdate act)
          >=> gsEditID              (pure . editIDUpdate act)
          >=> gsContributionState   (pure . contributionStateUpdate act)
          >=> gsHeaderState         (pure . headerStateUpdate act)
          >=> gsScreenState         (pure . maybe id screenStateUpdate (act ^? _ScreenAction))
          >=> gsMainMenuState       (pure . mainMenuUpdate act (isJust $ st ^. gsEditID))
          >=> gsTranslations        (pure . translationsUpdate act)
          >=> gsDevState            (pure . devStateUpdate act)
          >=> (\st' -> gsDocumentState (documentStateUpdate act st') st')

initRouting :: IO ()
initRouting = do
  Route.onLocationHashChange handleRouteChange
  handleRouteChange =<< Route.currentRoute

routesFromAction :: GlobalAction -> [Route.Route]
routesFromAction = \case
  LoadVDoc did                                    -> [Route.Process did]
  MainMenuAction (MainMenuActionOpen tab) -> case tab of
    MainMenuGroups{}                              -> [Route.Groups]
    MainMenuGroup k gid -> case k of
      MainMenuGroupProcesses                      -> [Route.GroupProcesses gid]
      MainMenuGroupMembers                        -> [Route.GroupMembers gid]
    MainMenuCreateOrUpdateGroup Nothing _         -> [Route.Groups]
    MainMenuCreateOrUpdateGroup (Just gid) _      -> [Route.GroupProcesses gid]
    MainMenuCreateProcess _                       -> [Route.Groups]
    MainMenuUpdateProcess pid _                   -> [Route.Process pid]
    MainMenuHelp                                  -> [Route.Help]
    MainMenuLogin k -> case k of
      MainMenuSubTabLogin                         -> [Route.Login]
      MainMenuSubTabRegistration                  -> [Route.Register]
  CompositeAction acts                            -> concatMap routesFromAction acts
  _                                               -> []

handleRouteChange :: Either Route.RouteParseError Route.Route -> IO ()
handleRouteChange r = do
  removeAllRanges
  case r of
    Right Route.Help                 -> exec . MainMenuAction . MainMenuActionOpen $ MainMenuHelp
    Right Route.Login                -> exec . MainMenuAction . MainMenuActionOpen $ MainMenuLogin MainMenuSubTabLogin
    Right Route.Register             -> exec . MainMenuAction . MainMenuActionOpen $ MainMenuLogin MainMenuSubTabRegistration
    Right Route.Groups               -> exec . MainMenuAction . MainMenuActionOpen $ MainMenuGroups ()
    Right (Route.GroupProcesses gid) -> exec . MainMenuAction . MainMenuActionOpen $ MainMenuGroup MainMenuGroupProcesses gid
    Right (Route.GroupMembers gid)   -> exec . MainMenuAction . MainMenuActionOpen $ MainMenuGroup MainMenuGroupMembers gid
    Right (Route.Process vid)        -> exec $ LoadVDoc vid
    Left _                           -> exec . MainMenuAction . MainMenuActionOpen $ defaultMainMenuTab
  where
    exec = executeAction . action @GlobalState

flushCacheMisses :: IO ()
flushCacheMisses = do
  keys <- nub <$> takeMVar cacheMissesMVar
  putMVar cacheMissesMVar []
  unless (null keys) $ do
    let msg = TSMissing keys
    sendTS msg
    consoleLogJSStringM "WS" . cs $ show msg

editIDUpdate :: GlobalAction -> Maybe (ID Common.VDoc) -> Maybe (ID Common.VDoc)
editIDUpdate (LoadVDoc cvd) _ = Just cvd
editIDUpdate _ st = st

serverCacheUpdate :: GlobalAction -> ServerCache -> ServerCache
serverCacheUpdate (CacheAction a) c = case a of
  RefreshServerCache c' -> c' <> c
  RestrictCacheItems keys -> restrictCache keys c
  InvalidateCacheItems keys -> invalidateCache keys c
serverCacheUpdate _ c = c


-- * logging state and actions.

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

consoleLogGlobalAction :: HasCallStack => forall m. MonadTransform m => GlobalAction -> m ()
consoleLogGlobalAction act | not (loggableAction act) = pure ()
consoleLogGlobalAction act = do
  let consolewidth = 80
      shown = show act
  if length shown <= consolewidth
    then do
      consoleLogJSStringM "Action: " (cs shown)
    else do
      consoleLogJSONM "Action: " act

loggableAction :: GlobalAction -> Bool
loggableAction (ContributionAction RequestSetAllVerticalSpanBounds) = False
loggableAction (ContributionAction SetAllVerticalSpanBounds{})      = False
loggableAction _                                                    = True


-- * pure updates

-- | Only touches the 'DevState' if it is 'Just'.  In production, 'gsDevState' should always be
-- 'Nothing'.  Use 'weAreInDevMode' to decide whether you want to initialize it, or, when writing
-- test cases, initialize it unconditionally (See `test/Refine/Frontend/Contribution/MarkSpec.hs`).
devStateUpdate :: HasCallStack => GlobalAction -> Maybe DevState -> Maybe DevState
devStateUpdate _ Nothing = Nothing
devStateUpdate act (Just devstate) = Just $ upd act devstate
  where
    upd a (DevState as) = DevState $ a : as


-- * ajax

emitBackendCallsFor :: HasCallStack => GlobalAction -> GlobalState -> IO ()
emitBackendCallsFor act st = case act of

    -- groups

    MainMenuAction (MainMenuActionOpen (MainMenuCreateOrUpdateGroup mid (FormComplete cg))) -> do
      case mid of
        Nothing -> do
          sendTS . TSAddGroup
                 $ cg & createGroupMembers %~ Map.fromList . map (first (^. userID)) . filter snd
          dispatchAndExec . MainMenuAction . MainMenuActionOpen $ MainMenuGroups ()
        Just gid -> do
          sendTS . TSUpdateGroup gid
                 $ cg & createGroupMembers %~ Map.fromList . map (first (^. userID))
          dispatchAndExec . MainMenuAction . MainMenuActionOpen $ MainMenuGroup MainMenuGroupProcesses gid

    MainMenuAction (MainMenuActionOpen (MainMenuCreateProcess (FormComplete cp))) -> do
      sendTS $ TSAddVDoc cp
      dispatchAndExec . MainMenuAction . MainMenuActionOpen . MainMenuGroup MainMenuGroupProcesses $ cp ^. createVDocGroup

    MainMenuAction (MainMenuActionOpen (MainMenuUpdateProcess vid (FormComplete cg))) -> do
      sendTS $ TSUpdateVDoc vid cg
      dispatchAndExec $ LoadVDoc vid

    -- contributions

    AddStatement upd sid statement -> sendTS $ (if upd then TSUpdateStatement else TSAddStatement) sid statement

    ContributionAction (SubmitComment (CommentInfo text kind)) -> do
      let headEdit = fromMaybe (error "emitBackendCallsFor.SubmitComment") . join
                   $ st ^. to gsEditID'
          range    = st ^? gsCurrentSelection . _Just . Common.selectionRange

      sendTS $ case kind of
        CommentKindDiscussion -> TSAddDiscussion headEdit $ Common.CreateDiscussion text range False
        CommentKindNote ->       TSAddDiscussion headEdit $ Common.CreateDiscussion text range True

      dispatchAndExec $ DocumentAction UpdateDocumentStateView

    DocumentAction (DocumentSave (FormBegin EditIsInitial))
      -> do
        let DocumentStateEdit editorState _ (Just baseEdit) = st ^. gsDocumentState
            cedit = Common.CreateEdit
                  { Common._createEditDesc        = "initial content"
                  , Common._createEditVDocVersion = getCurrentRawContent editorState
                  , Common._createEditKind        = Common.Initial
                  }

        sendTS $ TSAddEditAndMerge baseEdit cedit
        dispatchAndExec $ DocumentAction UpdateDocumentStateView

    DocumentAction (DocumentSave (FormComplete info))
      | DocumentStateEdit editorState _ baseEdit_ <- st ^. gsDocumentState
      -> do
        let baseEdit :: Common.ID Common.Edit
            (baseEdit:_) = catMaybes [baseEdit_, st ^? to gsEditID' . _Just . _Just]

            cedit = Common.CreateEdit
                  { Common._createEditDesc        = info ^. editInfoDesc
                  , Common._createEditVDocVersion = getCurrentRawContent editorState
                  , Common._createEditKind        = info ^. editInfoKind
                  }

        sendTS $ TSAddEdit baseEdit cedit
        dispatchAndExec $ DocumentAction UpdateDocumentStateView

    -- users

    CreateUser createUserData -> sendTS $ TSCreateUser createUserData

    -- voting

    ContributionAction (ToggleVoteOnContribution cid vote)
      -> sendTS $ TSToggleVote cid vote

    -- default

    _ -> pure ()


-- * triggering actions

instance Dispatchable GlobalAction where
  dispatch a = [action @GlobalState a]


-- * ranges and selections

-- FIXME: move this section to somewhere in Document.* modules, together with the Range type.

-- | See also: 'Range' type.  Empty selection (start point == end point) counts as no selection, and
-- triggers a 'ClearRange' action to be emitted.  Only call this in `readOnly` mode.
--
-- IO is needed for (1) going via the selection state in the browser api (@getSelection (dstate
-- ^. documentStateVal)@ would be nicer, but draft does not store selections in readOnly mode.), and
-- for (2) for looking at the DOM for the position data.
getRangeAction :: (HasCallStack, MonadIO m) => RawContent -> m (Maybe ContributionAction)
getRangeAction rc = do
  esel :: Either String Common.SelectionState <- runExceptT getDraftSelectionStateViaBrowser
  case esel of
    Left err -> do
      consoleLogJSONM "getRangeSelection: error" err
      pure $ Just ClearRange
    Right sel | rangeIsEmpty rc . Common._selectionRange $ Common.fromSelectionState rc sel -> do
      pure $ Just ClearRange
    Right sel -> Just . SetRange <$> do
      topOffset    <- liftIO js_getRangeTopOffset
      bottomOffset <- liftIO js_getRangeBottomOffset
      scrollOffset <- liftIO js_getScrollOffset
      let doctop = scrollOffset + if sel ^. Common.unSelectionState . Common.selectionIsBackward
                                  then topOffset
                                  else bottomOffset

      pure SelectionStateWithPx
        { _sstSelectionState = Common.fromSelectionState rc sel
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
