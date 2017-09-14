{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -Wno-orphans #-}

module Refine.Frontend.Document.Store
  ( EditorStore(..), emptyEditorStore, EditorStoreAction(..)
  , documentStateUpdate
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
import {-# SOURCE #-} Refine.Frontend.Store ()
import           Refine.Frontend.Test.Console
import           Refine.Frontend.Types
import           Refine.Frontend.Util


instance Dispatchable EditorStoreAction where
  dispatch a = [action @EditorStore a]

-- | Private global store of the 'draftEditor_' component.
instance StoreData EditorStore where
  type StoreAction EditorStore = EditorStoreAction
  transform = transformEditorStore

instance SometimesLoggable EditorStoreAction where
  shouldbeLogged _ = True

transformEditorStore :: EditorStoreAction -> EditorStore -> IO EditorStore
transformEditorStore act st = do
  consoleLogGlobalAction act
  st' <- transformEditorStoreHandler act st
  consoleLogGlobalStateWith (\(EditorStore (EditorState (NoJSONRep j))) -> j) (st /= st') st'
  pure st'

transformEditorStoreHandler :: EditorStoreAction -> EditorStore -> IO EditorStore
transformEditorStoreHandler = f where
  -- generic update.
  f (UpdateEditorStore es') _es = pure $ EditorStore es'

  -- toolbar buttons.
  f (DocumentToggleBlockType bt) (EditorStore es) = pure . EditorStore $ es & documentToggleBlockType bt
  f (DocumentToggleStyle s)      (EditorStore es) = pure . EditorStore $ es & documentToggleStyle s
  f DocumentRemoveLink           (EditorStore es) = pure . EditorStore $ es & documentRemoveLink
  f (DocumentCreateLink link)    (EditorStore es) = pure . EditorStore $ es & documentAddLink (cs link)
  f DocumentUndo                 (EditorStore es) = pure . EditorStore $ es & documentUndo
  f DocumentRedo                 (EditorStore es) = pure . EditorStore $ es & documentRedo

  -- open save dialog in 'GlobalState' (needs update with current 'EditorStore' content).
  f (DocumentRequestSave form) (EditorStore es) = do
    (dispatchAndExec . DocumentAction) `mapM_` [DocumentSave (FormBegin (form, es))]
    pure $ EditorStore es

-- | We pass two global states into this function, the one before the
-- pure update and the one after.  Using the one after is (was?)
-- sometimes necessary, but it's a bit dangerous because it can
-- trigger thunk evaluation loops.  use old state whenever it is
-- enough.
documentStateUpdate :: (HasCallStack)
                    => GlobalAction -> GlobalState -> DocumentState
                    -> CacheLookupT DocumentState
documentStateUpdate (LoadVDoc _) oldgs st
  = do
    cvdoc <- do
      case oldgs ^. gsCompositeVDoc of
        Nothing -> error "impossible - documentStateUdpate"
        Just Nothing -> throwError ()
        Just (Just x) -> pure x
    let edit = cvdoc ^. compositeVDocThisEdit
        needInitialContent = null $ edit ^. editSource . unEditSource
    pure $ if needInitialContent
        then enterEditMode oldgs (edit ^. editID)
        else enterViewMode cvdoc oldgs st

-- in view mode, start edit
documentStateUpdate (HeaderAction StartEdit) oldgs@(view gsEditID -> Just (Just eid)) (DocumentStateView ())
  = pure $ enterEditMode oldgs eid

-- in diff mode, start edit
documentStateUpdate (HeaderAction StartEdit) oldgs (DocumentStateDiff _ _ eid _ _)
  = pure $ enterEditMode oldgs eid

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    oldgs
                    (DocumentStateView r)
  = pure $ DocumentStateDiff
      (mkEditIndex (fromMaybe (error "impossible @documentStateUpdate") . join $ oldgs ^. gsEdit) eid)
      r
      eid
      True
      ()

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit _)))
                    _oldgs
                    (DocumentStateDiff _ r _ _ _)
  = pure $ DocumentStateView r

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDDiscussion _ did)))
                    _oldgs
                    (DocumentStateView _)
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
                    (DocumentStateDiff _ r _ _ _)
  = pure $ DocumentStateView r

documentStateUpdate (DocumentAction UpdateDocumentStateView) _ _
  = pure $ DocumentStateView ()

documentStateUpdate (DocumentAction (DocumentUpdateEditInfo info)) _ st
  = pure $ st & documentStateEditInfo .~ info

{- FIXME: #451
documentStateUpdate (ContributionAction (SetRange range)) gs@(view gsVDocID -> Just{}) _
  = pure
  . mkDocumentStateView
  . addMarksToRawContent [(MarkCurrentSelection, range ^. sstSelectionState . selectionRange)]
  . deleteMarksFromRawContentIf (== MarkCurrentSelection)
  $ gsRawContent gs

documentStateUpdate (ContributionAction ClearRange) gs@(view gsVDocID -> Just{}) _
  = pure
  . mkDocumentStateView
  . deleteMarksFromRawContentIf (== MarkCurrentSelection)
  $ gsRawContent gs
-}

documentStateUpdate (DocumentAction ToggleCollapseDiff) _ st | has _DocumentStateDiff st
  = pure $ st & documentStateDiffCollapsed %~ not

documentStateUpdate (HeaderAction ToggleIndexToolbarExtension) _ st | has _DocumentStateDiff st
  = pure $ st & documentStateDiffCollapsed .~ False

documentStateUpdate _ _ st
  = pure st


enterViewMode :: CompositeVDoc -> GlobalState -> DocumentState -> DocumentState
enterViewMode cvdoc oldgs = refreshDocumentStateView ed eidChanged
  where
    ed = fromMaybe (error "impossible @enterViewMode") . join $ oldgs ^. gsEdit
    eidChanged = Just newID /= mOldID
      where
        newID  = cvdoc ^. compositeVDocThisEditID
        mOldID = oldgs ^? gsEditID . _Just . _Just

enterEditMode :: GlobalState -> ID Edit -> DocumentState
enterEditMode oldgs eid = case cacheLookup oldgs eid of
  Just ed
    -> let einfo = EditInfo (ed ^. editDesc) (Just $ ed ^. editKind) $
                     newLocalStateRef (EditInputState einfo Nothing) oldgs
       in DocumentStateEdit einfo (Just eid)
  Nothing
    -> error $ "enterEditMode: " <> show eid

-- | construct a 'SetAllVerticalSpanBounds' action.
setAllVerticalSpanBounds :: (HasCallStack, MonadIO m) => RawContent -> m ContributionAction
setAllVerticalSpanBounds rawContent = liftIO $ do
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

    SetAllVerticalSpanBounds . concat <$> (getPos `mapM` Map.toList marks)

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
