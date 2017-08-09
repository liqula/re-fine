{-# LANGUAGE BangPatterns               #-}
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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Document.Store
  ( documentStateUpdate
  , setAllVerticalSpanBounds
  ) where

import Refine.Frontend.Prelude

import qualified Data.Map as Map

import           Refine.Common.Types
import           React.Flux.Missing
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
documentStateUpdate :: HasCallStack => GlobalAction -> GlobalState -> GlobalState -> GlobalDocumentState -> GlobalDocumentState
documentStateUpdate (LoadVDoc (AfterAjax vdoc)) oldgs _newgs st
  = let eidChanged = Just newID /= mOldID
        newID  = vdoc ^. vdocHeadEdit
        mOldID = oldgs ^? gsVDoc . _Just . compositeVDocThisEditID
        oldEdit = fromMaybe (error "impossible") $ gsEdit oldgs
    in refreshDocumentStateView
         oldEdit
         eidChanged
         (oldEdit ^. editVDocVersion)  -- (really, get the vdoc version from the old gs here?)
         st

documentStateUpdate (LoadCompositeVDoc (AfterAjax cvdoc)) oldgs _newgs st
  = if needInitialContent
      then enterEditModeWithEditorState oldgs estate (Just $ cvdoc ^. compositeVDocThisEdit . editID)
      else enterViewMode cvdoc oldgs st
  where
    needInitialContent = null $ cvdoc ^. compositeVDocThisEdit . editSource . unEditSource
    estate = cvdoc ^. compositeVDocThisEdit . editVDocVersion . to createWithRawContent

documentStateUpdate (DocumentAction (DocumentSave _)) _ (view gsVDoc -> Just cvdoc) DocumentStateEdit{}
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (HeaderAction StartEdit) oldgs _ (DocumentStateView estate _)
  = enterEditModeWithEditorState oldgs estate Nothing

documentStateUpdate (HeaderAction StartEdit) oldgs _ (DocumentStateDiff _ _ _ edit _ _)
  = enterEditModeWithEdit oldgs edit

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    oldgs
                    _newgs
                    (DocumentStateView e r)
  = DocumentStateDiff
      (mkEditIndex (fromMaybe (error "impossible") $ gsEdit oldgs) eid)
      e
      r
      eid
      True
      ()

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit _)))
                    _oldgs
                    _newgs
                    (DocumentStateDiff _ e r _ _ _)
  = DocumentStateView e r

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDDiscussion did)))
                    _oldgs
                    _newgs
                    (DocumentStateView _ _)
  = DocumentStateDiscussion (did, Nothing)

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDDiscussion _)))
                    _oldgs
                    (view gsVDoc -> Just cvdoc)
                    (DocumentStateDiscussion _)
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction (ReplyStatement upd sid (FormBegin lst)))
                    _oldgs
                    _newgs
                    (DocumentStateDiscussion (did, Nothing))
  = DocumentStateDiscussion (did, Just (StatementEditorProps sid lst upd))

documentStateUpdate (DocumentAction (ReplyStatement _upd _sid FormCancel))
                    _oldgs
                    _newgs
                    (DocumentStateDiscussion (did, Just _))
  = DocumentStateDiscussion (did, Nothing)

documentStateUpdate (AddStatement _upd _sid (AfterAjax discussion))
                    _oldgs
                    _newgs
                    (DocumentStateDiscussion _)
  = DocumentStateDiscussion (discussion ^. discussionID, Nothing)

documentStateUpdate (ContributionAction HideContributionDialog)
                    _oldgs
                    _newgs
                    (DocumentStateDiff _ e r _ _ _)
  = DocumentStateView e r

documentStateUpdate (DocumentAction (UpdateEditorState estate)) _ _ (DocumentStateEdit _ einfo base)
  = DocumentStateEdit estate einfo base

documentStateUpdate (DocumentAction (DocumentUpdateEditInfo info)) _ _ st
  = st & documentStateEditInfo .~ info

documentStateUpdate (DocumentAction (DocumentToggleBlockType bt)) _ _ st
  = st & documentStateVal %~ documentToggleBlockType bt

documentStateUpdate (DocumentAction (DocumentToggleStyle s)) _ _ st
  = st & documentStateVal %~ documentToggleStyle s

documentStateUpdate (DocumentAction DocumentRemoveLink) _ _ st
  = st & documentStateVal %~ documentRemoveLink

documentStateUpdate (DocumentAction (DocumentCreateLink link)) _ _ st
  = st & documentStateVal %~ documentAddLink (cs link)

documentStateUpdate (DocumentAction DocumentUndo) _ _ st
  = st & documentStateVal %~ documentUndo

documentStateUpdate (DocumentAction DocumentRedo) _ _ st
  = st & documentStateVal %~ documentRedo

documentStateUpdate (AddDiscussion _) _ (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddNote _) _ (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddEdit _) _ (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction DocumentCancelSave) _ (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (ContributionAction (SetRange range)) _ (view gsVDoc -> Just cvdoc) _
  = mkDocumentStateView
  . addMarksToRawContent [(MarkCurrentSelection, range ^. sstSelectionState . selectionRange)]
  . deleteMarksFromRawContentIf (== MarkCurrentSelection)
  $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (ContributionAction ClearRange) _ (view gsVDoc -> Just cvdoc) _
  = mkDocumentStateView
  . deleteMarksFromRawContentIf (== MarkCurrentSelection)
  $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction ToggleCollapseDiff) _ _ st | has _DocumentStateDiff st
  = st & documentStateDiffCollapsed %~ not

documentStateUpdate (HeaderAction ToggleIndexToolbarExtension) _ _ st | has _DocumentStateDiff st
  = st & documentStateDiffCollapsed .~ False

documentStateUpdate _ _ _ st
  = st


enterViewMode :: CompositeVDoc -> GlobalState -> GlobalDocumentState -> GlobalDocumentState
enterViewMode cvdoc oldgs = refreshDocumentStateView ed eidChanged rc
  where
    ed = fromMaybe (error "impossible") $ gsEdit oldgs
    eidChanged = Just newID /= mOldID
      where
        newID  = cvdoc ^. compositeVDocThisEditID
        mOldID = oldgs ^? gsVDoc . _Just . compositeVDocThisEditID
    rc = rawContentFromCompositeVDoc cvdoc

-- | i think this is for when an edit is already under way, and we want to refresh the edit mode,
-- like after opening and closing the save dialog?
enterEditModeWithEdit :: GlobalState -> ID Edit -> GlobalDocumentState
enterEditModeWithEdit oldgs eid = DocumentStateEdit
      (createWithRawContent $ ed ^. editVDocVersion)
      einfo
      (Just eid)
  where
    Just ed = getEdit oldgs eid

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
