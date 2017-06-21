{-# LANGUAGE NoImplicitPrelude          #-}
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


documentStateUpdate :: GlobalAction -> GlobalState -> DocumentState -> DocumentState
documentStateUpdate (OpenDocument cvdoc) _ _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction (DocumentSave _)) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc  -- FIXME: store last state before edit in DocumentStateEdit, and restore it from there?

documentStateUpdate (HeaderAction (StartEdit kind)) gs (DocumentStateView estate _)
  = DocumentStateEdit (forceSelection estate (toSelectionState $ gs ^. gsCurrentSelection)) kind

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    (view gsVDoc -> Just cvdoc)
                    (DocumentStateView e r)
  = DocumentStateDiff e r (cvdoc ^?! compositeVDocApplicableEdits . ix eid) True

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit _)))
                    _
                    (DocumentStateDiff e r _ _)
  = DocumentStateView e r

documentStateUpdate (ContributionAction HideContributionDialog)
                    _
                    (DocumentStateDiff e r _ _)
  = DocumentStateView e r

documentStateUpdate (DocumentAction (DocumentUpdate state')) _ _state
  = state'

documentStateUpdate (DocumentAction (DocumentUpdateEditKind kind)) _ st
  = st & documentStateEditKind .~ kind

documentStateUpdate (DocumentAction (DocumentToggleBlockType bt)) _ st
  = st & documentStateVal %~ documentToggleBlockType bt

documentStateUpdate (DocumentAction (DocumentToggleStyle s)) _ st
  = st & documentStateVal %~ documentToggleStyle s

documentStateUpdate (DocumentAction DocumentRemoveLink) _ st
  = st & documentStateVal %~ documentRemoveLink

documentStateUpdate (DocumentAction (DocumentCreateLink link)) _ st
  = st & documentStateVal %~ documentAddLink (cs link)

documentStateUpdate (DocumentAction DocumentUndo) _ st
  = st & documentStateVal %~ documentUndo

documentStateUpdate (DocumentAction DocumentRedo) _ st
  = st & documentStateVal %~ documentRedo

documentStateUpdate (AddDiscussion _) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddNote _) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddEdit _) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction DocumentCancelSave) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (ContributionAction (SetRange range)) _ ((^? documentStateContent) -> Just rc)
  = mkDocumentStateView
  . addMarksToRawContent [(ContribIDHighlightMark, range ^. sstSelectionState . selectionRange)]
  . deleteMarksFromRawContentIf (== ContribIDHighlightMark)
  $ rc

documentStateUpdate (ContributionAction ClearRange) _ ((^? documentStateContent) -> Just rc)
  = mkDocumentStateView
  . deleteMarksFromRawContentIf (== ContribIDHighlightMark)
  $ rc

documentStateUpdate (DocumentAction ToggleCollapseDiff) _ st | has _DocumentStateDiff st
  = st & documentStateDiffCollapsed %~ not

documentStateUpdate _ _ st
  = st


editorStateToVDocVersion :: EditorState -> VDocVersion
editorStateToVDocVersion = rawContentToVDocVersion . convertToRaw . getCurrentContent

editorStateFromVDocVersion :: VDocVersion -> EditorState
editorStateFromVDocVersion = createWithContent . convertFromRaw . rawContentFromVDocVersion

-- | construct a 'SetAllVertialSpanBounds' action.
setAllVertialSpanBounds :: MonadIO m => DocumentState -> m ContributionAction
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
