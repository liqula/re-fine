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
  , editorStateToVDocVersion
  , editorStateFromVDocVersion
  , setAllVertialSpanBounds
  ) where

import Refine.Frontend.Prelude

import           Control.Lens (ix)
import qualified Data.Map as Map

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
documentStateUpdate :: HasCallStack => GlobalAction -> GlobalState -> GlobalState -> DocumentState -> DocumentState
documentStateUpdate (OpenDocument cvdoc) _oldgs _newgs _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction (DocumentSave _)) _ (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc  -- FIXME: store last state before edit in DocumentStateEdit, and restore it from there?

documentStateUpdate (HeaderAction (StartEdit kind)) _ gs (DocumentStateView estate _)
  = DocumentStateEdit (forceSelection estate (toSelectionState $ gs ^. gsCurrentSelection)) kind

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    _oldgs
                    (view gsVDoc -> Just cvdoc)
                    (DocumentStateView e r)
  = DocumentStateDiff e r (cvdoc ^?! compositeVDocApplicableEdits . ix eid) True

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit _)))
                    _oldgs
                    _newgs
                    (DocumentStateDiff e r _ _)
  = DocumentStateView e r

documentStateUpdate (ContributionAction HideContributionDialog)
                    _oldgs
                    _newgs
                    (DocumentStateDiff e r _ _)
  = DocumentStateView e r

documentStateUpdate (DocumentAction (DocumentUpdate state')) _ _ _state
  = state'

documentStateUpdate (DocumentAction (DocumentUpdateEditKind kind)) _ _ st
  = st & documentStateEditKind .~ kind

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

documentStateUpdate (ContributionAction (SetRange range)) _ _ ((^? documentStateContent) -> Just rc)
  = mkDocumentStateView
  . addMarksToRawContent [(ContribIDHighlightMark, range ^. sstSelectionState . selectionRange)]
  . deleteMarksFromRawContentIf (== ContribIDHighlightMark)
  $ rc

documentStateUpdate (ContributionAction ClearRange) _ _ ((^? documentStateContent) -> Just rc)
  = mkDocumentStateView
  . deleteMarksFromRawContentIf (== ContribIDHighlightMark)
  $ rc

documentStateUpdate (DocumentAction ToggleCollapseDiff) _ _ st | has _DocumentStateDiff st
  = st & documentStateDiffCollapsed %~ not

documentStateUpdate _ _ _ st
  = st


editorStateToVDocVersion :: HasCallStack => EditorState -> VDocVersion
editorStateToVDocVersion = rawContentToVDocVersion . convertToRaw . getCurrentContent

editorStateFromVDocVersion :: HasCallStack => VDocVersion -> EditorState
editorStateFromVDocVersion = createWithContent . convertFromRaw . rawContentFromVDocVersion

-- | construct a 'SetAllVertialSpanBounds' action.
setAllVertialSpanBounds :: HasCallStack => MonadIO m => DocumentState -> m ContributionAction
setAllVertialSpanBounds (convertToRaw . getCurrentContent . view documentStateVal -> rawContent) = liftIO $ do
    let marks :: Map ContributionID (Ranges LeafSelector)
        marks = getLeafSelectors rawContent

        getPos :: (ContributionID, Ranges LeafSelector) -> IO [(ContributionID, VertialSpanBounds)]
        getPos (cid, rs) = forM (unRanges rs) $ \(Range top bot) -> do
          topOffset    <- OffsetFromViewportTop  <$> getLeafSelectorBound LeafSelectorTop    top
          bottomOffset <- OffsetFromViewportTop  <$> getLeafSelectorBound LeafSelectorBottom bot
          scrollOffset <- ScrollOffsetOfViewport <$> js_getScrollOffset
          let vertialSpanBounds = VertialSpanBounds
                { _vertialSpanBoundsTop    = offsetFromDocumentTop topOffset    scrollOffset
                , _vertialSpanBoundsBottom = offsetFromDocumentTop bottomOffset scrollOffset
                }
          pure (cid, vertialSpanBounds)

    SetAllVertialSpanBounds . concat <$> (getPos `mapM` Map.toList marks)
