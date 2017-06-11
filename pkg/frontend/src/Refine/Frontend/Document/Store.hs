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
  , setMarkPositions
  ) where

import Refine.Frontend.Prelude

import           Control.Lens (ix)

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Common.VDoc.OT (docRanges)
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
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

documentStateUpdate (HeaderAction (StartEdit kind)) gs (DocumentStateView estate rc)
  = let sel = fromMaybe (selectEverything rc)
            $ chunkRangeToSelectionState (convertToRaw $ getCurrentContent estate)  -- FIXME: #312
          <$> gs ^? gsContributionState . csCurrentRange . _Just . rangeSelectionState
    in DocumentStateEdit (forceSelection estate sel) kind

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    (view gsVDoc -> Just cvdoc)
                    (DocumentStateView e r)
  = DocumentStateDiff e r (cvdoc ^?! compositeVDocEdits . ix eid) True

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

documentStateUpdate (DocumentAction DocumentToggleLink) _ st
  = st & documentStateVal %~ documentToggleLink

documentStateUpdate (AddDiscussion _) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddNote _) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddEdit _) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction DocumentCancelSave) (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (ContributionAction (SetRange (view rangeSelectionState -> sel))) _ ((^? documentStateContent) -> Just rc)
  = mkDocumentStateView
  . addMarksToRawContent [(ContribIDHighlightMark, chunkRangeToSelectionState rc sel)]
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


-- | construct a 'SetMarkPositions' action.
setMarkPositions :: MonadIO m => DocumentState -> m ContributionAction
setMarkPositions (convertToRaw . getCurrentContent . view documentStateVal -> rawContent) = liftIO $ do
    let marks :: [(ContributionID, MarkSelector, MarkSelector)]
        marks = getMarkSelectors rawContent

        getPos :: (ContributionID, MarkSelector, MarkSelector) -> IO (ContributionID, MarkPosition)
        getPos (cid, top, bot) = do
          topOffset    <- OffsetFromViewportTop  <$> getMarkSelectorBound top
          bottomOffset <- OffsetFromViewportTop  <$> getMarkSelectorBound bot
          scrollOffset <- ScrollOffsetOfViewport <$> js_getScrollOffset
          let markPosition = MarkPosition
                { _markPositionTop    = offsetFromDocumentTop topOffset    scrollOffset
                , _markPositionBottom = offsetFromDocumentTop bottomOffset scrollOffset
                }
          pure (cid, markPosition)

    SetMarkPositions <$> (getPos `mapM` marks)

documentToggleLink :: EditorState -> EditorState
documentToggleLink st
    | selectionIsEmpty rc sel = st
-- TODO    | not (null ([sel] `intersectSelections` linkranges)) = documentRemoveLink st
    | otherwise = documentAddLink link st
  where
    _linkranges = docRanges (\((Atom l, _), _) -> isLink l) (rawContentToDoc rc)
    sel = getSelection st
    rc = convertToRaw $ getCurrentContent st

    isLink (Just (EntityLink _)) = True
    isLink _ = False

    link = "http://www.example.com"  -- TODO

-- TODO
-- intersectSelections :: [SelectionState] -> [SelectionState] -> [SelectionState]
