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
  , editorStateToVDocVersion
  , editorStateFromVDocVersion
  , setAllVerticalSpanBounds
  ) where

import Refine.Frontend.Prelude

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
documentStateUpdate :: HasCallStack => GlobalAction -> GlobalState -> GlobalState -> GlobalDocumentState -> GlobalDocumentState
documentStateUpdate (OpenDocument cvdoc) oldgs _newgs st
  = let eidChanged = Just newID /= mOldID
        newID  = cvdoc ^. compositeVDocThisEditID
        mOldID = oldgs ^? gsVDoc . _Just . compositeVDocThisEditID
    in refreshDocumentStateView eidChanged st

documentStateUpdate (DocumentAction (DocumentSave _)) _ (view gsVDoc -> Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (HeaderAction StartEdit) _oldgs _ (DocumentStateView _)
  = DocumentStateEdit (EditInfo "" Nothing)

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit eid)))
                    _oldgs
                    _newgs
                    (DocumentStateView r)
  = DocumentStateDiff r eid True

documentStateUpdate (ContributionAction (ShowContributionDialog (ContribIDEdit _)))
                    _oldgs
                    _newgs
                    (DocumentStateDiff r _ _)
  = DocumentStateView r

documentStateUpdate (ContributionAction HideContributionDialog)
                    _oldgs
                    _newgs
                    (DocumentStateDiff r _ _)
  = DocumentStateView r

documentStateUpdate (DocumentAction (DocumentUpdate state')) _ _ _state
  = state'

documentStateUpdate (DocumentAction (DocumentUpdateEditInfo info)) _ _ st
  = st & documentStateEditInfo .~ info

{-

hm, how do we do these?  write diffs in st, then on rendering, read them and send another action
'FlushEditoStateUpdateQueue'?

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
-}

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
  . addMarksToRawContent [(ContribIDHighlightMark, range ^. sstSelectionState . selectionRange)]
  . deleteMarksFromRawContentIf (== ContribIDHighlightMark)
  $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (ContributionAction ClearRange) _ (view gsVDoc -> Just cvdoc) _
  = mkDocumentStateView
  . deleteMarksFromRawContentIf (== ContribIDHighlightMark)
  $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction ToggleCollapseDiff) _ _ st | has _DocumentStateDiff st
  = st & documentStateDiffCollapsed %~ not

documentStateUpdate _ _ _ st
  = st


editorStateToVDocVersion :: HasCallStack => EditorState -> VDocVersion
editorStateToVDocVersion = rawContentToVDocVersion . convertToRaw . getCurrentContent

editorStateFromVDocVersion :: HasCallStack => VDocVersion -> EditorState
editorStateFromVDocVersion = createWithContent . convertFromRaw . rawContentFromVDocVersion

-- | construct a 'SetAllVerticalSpanBounds' action.
setAllVerticalSpanBounds :: (HasCallStack, MonadIO m) => DocumentState_ a b -> m ContributionAction
setAllVerticalSpanBounds _ = liftIO $ do
    let marks :: Map ContributionID (Ranges LeafSelector)
        marks = getLeafSelectors undefined  -- RequestSetAllVerticalSpanBounds could carry a fresh
                                            -- copy of EditorState?  no need to keep it in the
                                            -- store?

        getPos :: (ContributionID, Ranges LeafSelector) -> IO [(ContributionID, VerticalSpanBounds)]
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
