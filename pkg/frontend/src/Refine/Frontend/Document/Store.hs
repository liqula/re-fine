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

import           Control.Lens ((&), (%~))

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Screen.Calculations
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util


documentStateUpdate :: GlobalAction -> Maybe CompositeVDoc -> DocumentState -> DocumentState
documentStateUpdate (OpenDocument cvdoc) _ _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction DocumentSave) (Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc  -- FIXME: store last state before edit in DocumentStateEdit, and restore it from there?

documentStateUpdate (HeaderAction (StartEdit kind)) _ (DocumentStateView _ estate)
  = DocumentStateEdit estate kind

documentStateUpdate (DocumentAction (DocumentUpdate state')) _ _state
  = state'

documentStateUpdate (DocumentAction DocumentToggleBold) _ state
  = state & documentStateVal %~ documentToggleBold

documentStateUpdate (DocumentAction DocumentToggleItalic) _ state
  = state & documentStateVal %~ documentToggleItalic

documentStateUpdate (AddDiscussion _) (Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddNote _) (Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (AddEdit _) (Just cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate _ _ state
  = state


editorStateToVDocVersion :: EditorState -> VDocVersion
editorStateToVDocVersion = rawContentToVDocVersion . convertToRaw . getCurrentContent

editorStateFromVDocVersion :: VDocVersion -> EditorState
editorStateFromVDocVersion = createWithContent . convertFromRaw . rawContentFromVDocVersion


-- | construct a 'SetMarkPositions' action.
setMarkPositions :: RawContent -> IO ContributionAction
setMarkPositions rawContent = do
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
