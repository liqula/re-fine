{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Document.Store
  ( documentStateUpdate
  , setAllVerticalSpanBounds
  ) where
#include "import_frontend.hs"

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.FFI.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Screen.Calculations
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- | We pass two global states into this function, the one before the
-- pure update and the one after.  Using the one after is (was?)
-- sometimes necessary, but it's a bit dangerous because it can
-- trigger thunk evaluation loops.  use old state whenever it is
-- enough.
documentStateUpdate :: (HasCallStack)
                    => GlobalAction -> GlobalState -> GlobalDocumentState
                    -> CacheLookupT GlobalDocumentState
documentStateUpdate (LoadVDoc _) oldgs st
  = do
    cvdoc <- do
      case oldgs ^. gsVDoc of
        Nothing -> error "impossible - documentStateUdpate"
        Just Nothing -> throwError ()
        Just (Just x) -> pure x
    let edit = cvdoc ^. compositeVDocThisEdit
        needInitialContent = null $ edit ^. editSource . unEditSource
        estate = edit ^. editVDocVersion . to createWithRawContent
    pure $ if needInitialContent
        then enterEditModeWithEditorState oldgs estate (Just $ edit ^. editID)
        else enterViewMode cvdoc oldgs st

documentStateUpdate (HeaderAction StartEdit) oldgs (DocumentStateView estate _)
  = pure $ enterEditModeWithEditorState oldgs estate Nothing

documentStateUpdate (HeaderAction StartEdit) oldgs (DocumentStateDiff _ _ _ edit _ _)
  = pure $ enterEditModeWithEdit oldgs edit

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    oldgs
                    (DocumentStateView e r)
  = pure $ DocumentStateDiff
      (mkEditIndex (fromMaybe (error "impossible @documentStateUpdate") . join $ gsEdit oldgs) eid)
      e
      r
      eid
      True
      ()

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit _)))
                    _oldgs
                    (DocumentStateDiff _ e r _ _ _)
  = pure $ DocumentStateView e r

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDDiscussion _ did)))
                    _oldgs
                    (DocumentStateView _ _)
  = pure $ DocumentStateDiscussion (did, Nothing)

documentStateUpdate (DocumentAction (ReplyStatement upd sid (FormBegin lst)))
                    _oldgs
                    (DocumentStateDiscussion (did, Nothing))
  = pure $ DocumentStateDiscussion (did, Just (StatementEditorProps sid lst upd))

documentStateUpdate (DocumentAction (ReplyStatement _upd _sid FormCancel))
                    _oldgs
                    (DocumentStateDiscussion (did, Just _))
  = pure $ DocumentStateDiscussion (did, Nothing)

documentStateUpdate (AddStatement _upd _sid _discussion)
                    _oldgs
                    (DocumentStateDiscussion (did, _))
  = pure $ DocumentStateDiscussion (did, Nothing)

documentStateUpdate (ContributionAction HideContributionDialog)
                    _oldgs
                    (DocumentStateDiff _ e r _ _ _)
  = pure $ DocumentStateView e r

documentStateUpdate (DocumentAction (UpdateEditorState estate)) _ (DocumentStateEdit _ einfo base)
  = pure $ DocumentStateEdit estate einfo base

documentStateUpdate (DocumentAction UpdateDocumentStateView) gs@(view gsEditID -> Just{}) _state
  = pure . mkDocumentStateView $ gsRawContent gs

documentStateUpdate (DocumentAction (DocumentUpdateEditInfo info)) _ st
  = pure $ st & documentStateEditInfo .~ info

documentStateUpdate (DocumentAction (DocumentToggleBlockType bt)) _ st
  = pure $ st & documentStateVal %~ documentToggleBlockType bt

documentStateUpdate (DocumentAction (DocumentToggleStyle s)) _ st
  = pure $ st & documentStateVal %~ documentToggleStyle s

documentStateUpdate (DocumentAction DocumentRemoveLink) _ st
  = pure $ st & documentStateVal %~ documentRemoveLink

documentStateUpdate (DocumentAction (DocumentCreateLink link)) _ st
  = pure $ st & documentStateVal %~ documentAddLink (cs link)

documentStateUpdate (DocumentAction DocumentUndo) _ st
  = pure $ st & documentStateVal %~ documentUndo

documentStateUpdate (DocumentAction DocumentRedo) _ st
  = pure $ st & documentStateVal %~ documentRedo

documentStateUpdate (ContributionAction (SetRange range)) gs@(view gsEditID -> Just{}) _
  = pure
  . mkDocumentStateView
  . addMarksToRawContent [(MarkCurrentSelection, range ^. sstSelectionState . selectionRange)]
  . deleteMarksFromRawContentIf (== MarkCurrentSelection)
  $ gsRawContent gs

documentStateUpdate (ContributionAction ClearRange) gs@(view gsEditID -> Just{}) _
  = pure
  . mkDocumentStateView
  . deleteMarksFromRawContentIf (== MarkCurrentSelection)
  $ gsRawContent gs

documentStateUpdate (DocumentAction ToggleCollapseDiff) _ st | has _DocumentStateDiff st
  = pure $ st & documentStateDiffCollapsed %~ not

documentStateUpdate (HeaderAction ToggleIndexToolbarExtension) _ st | has _DocumentStateDiff st
  = pure $ st & documentStateDiffCollapsed .~ False

documentStateUpdate CacheAction{} (view gsVDoc -> Just (Just cvdoc)) st | has _DocumentStateView st || has _DocumentStateDiff st
  = pure $ st & documentStateVal .~ createWithContent (convertFromRaw $ rawContentFromCompositeVDoc cvdoc)

documentStateUpdate _ _ st
  = pure st


enterViewMode :: CompositeVDoc -> GlobalState -> GlobalDocumentState -> GlobalDocumentState
enterViewMode cvdoc oldgs = refreshDocumentStateView ed eidChanged rc
  where
    ed = fromMaybe (error "impossible @enterViewMode") . join $ gsEdit oldgs
    eidChanged = Just newID /= mOldID
      where
        newID  = cvdoc ^. compositeVDocThisEditID
        mOldID = oldgs ^? to gsEditID' . _Just . _Just
    rc = rawContentFromCompositeVDoc cvdoc

-- | i think this is for when an edit is already under way, and we want to refresh the edit mode,
-- like after opening and closing the save dialog?
enterEditModeWithEdit :: GlobalState -> ID Edit -> GlobalDocumentState
enterEditModeWithEdit oldgs eid = DocumentStateEdit
      (createWithRawContent $ ed ^. editVDocVersion)
      einfo
      (Just eid)
  where
    Just ed = cacheLookup oldgs eid

    einfo = EditInfo
             (ed ^. editDesc)
             (Just $ ed ^. editKind)
             $ newLocalStateRef (EditInputState einfo Nothing) oldgs

enterEditModeWithEditorState :: GlobalState -> EditorState -> Maybe (ID Edit) -> GlobalDocumentState
enterEditModeWithEditorState oldgs estate = DocumentStateEdit
      (maybe estate (forceSelection estate . (`toSelectionState` True)) $ oldgs ^. gsCurrentSelection)
      einfo
  where
    einfo = EditInfo "" Nothing $ newLocalStateRef (EditInputState einfo Nothing) oldgs


-- | construct a 'SetAllVerticalSpanBounds' action.
setAllVerticalSpanBounds :: (HasCallStack, MonadIO m) => DocumentState_ a b c d -> m (Maybe ContributionAction)
setAllVerticalSpanBounds ((^? documentStateVal . to getCurrentContent . to convertToRaw) -> Just rawContent) = liftIO $ do
    let marks :: Map MarkID (Ranges LeafSelector)
        marks = getLeafSelectors rawContent

        getPos :: (MarkID, Ranges LeafSelector) -> IO [(MarkID, VerticalSpanBounds)]
        getPos (cid, rs) = fmap catMaybes . forM (unRanges rs) $ \(Range top bot) -> do
          mb <- getLeafSelectorBound LeafSelectorTop top
          case mb of
            Nothing -> pure Nothing
            Just _ -> Just <$> do
              assertLeafSelector `mapM_` [top, bot]
              -- (FIXME: this assertion should be compiled away in
              -- production.  write a few assert functions that can do
              -- that, and use one of those.)

              let fromJust_ = fromMaybe (error "setAllVerticalSpanBounds: internal error.")

              topOffset    <- OffsetFromViewportTop  . fromJust_ <$> getLeafSelectorBound LeafSelectorTop    top
              bottomOffset <- OffsetFromViewportTop  . fromJust_ <$> getLeafSelectorBound LeafSelectorBottom bot
              scrollOffset <- ScrollOffsetOfViewport            <$> js_getScrollOffset
              let verticalSpanBounds = VerticalSpanBounds
                    { _verticalSpanBoundsTop    = offsetFromDocumentTop topOffset    scrollOffset
                    , _verticalSpanBoundsBottom = offsetFromDocumentTop bottomOffset scrollOffset
                    }
              pure (cid, verticalSpanBounds)

    Just . SetAllVerticalSpanBounds . concat <$> (getPos `mapM` Map.toList marks)
setAllVerticalSpanBounds _ = pure Nothing

assertLeafSelector :: HasCallStack => LeafSelector -> IO ()
assertLeafSelector sel = void (getLeafSelectorBound LeafSelectorTop sel) `catch` \(JSException _ msg) -> do
  htmlContent <- js_assertLeafSelectorDebug
  throwIO . ErrorCall . unlines $ [msg, show sel, cs htmlContent]

#ifdef __GHCJS__

foreign import javascript safe
  "$r = document.querySelector('article').innerHTML"
  js_assertLeafSelectorDebug :: IO JSString

#else

{-# ANN js_assertLeafSelectorDebug ("HLint: ignore Use camelCase" :: String) #-}
js_assertLeafSelectorDebug :: IO JSString
js_assertLeafSelectorDebug = error "javascript FFI not available in GHC"

#endif
