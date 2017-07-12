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

module Refine.Frontend.Document.Types where

import Refine.Frontend.Prelude

import           GHC.Generics (Generic)
import           React.Flux (UnoverlapAllEq)

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Contribution.Types


data DocumentAction =
    DocumentUpdate GlobalDocumentState
  | DocumentUpdateEditInfo (EditInfo (Maybe EditKind))
  | RequestDocumentSave
  | DocumentSave (EditInfo EditKind)
  | DocumentCancelSave
  | DocumentToggleStyle Style
  | DocumentToggleBlockType BlockType
  | DocumentRemoveLink
  | DocumentCreateLink ST
  | ToggleCollapseDiff
  | DocumentUndo
  | DocumentRedo
  deriving (Show, Eq, Generic)

data DocumentState_ edit =
    DocumentStateView
  | DocumentStateDiff
      { _documentStateDiff          :: edit
      , _documentStateDiffCollapsed :: Bool
      }
  | DocumentStateEdit
      { _documentStateEditInfo :: EditInfo (Maybe EditKind)  -- ^ 'editInput_' dialog state lives here between close / re-open.
      }
  deriving (Show, Eq, Generic, Functor)

mapDocumentState :: (b -> b') -> DocumentState_ b -> DocumentState_ b'
mapDocumentState g = \case
  DocumentStateView -> DocumentStateView
  DocumentStateDiff e y -> DocumentStateDiff (g e) y
  DocumentStateEdit y -> DocumentStateEdit y

-- | The document state variant for 'DocumentProps'.
type DocumentState = DocumentState_ Edit

-- | The document state for 'GlobalState'.
type GlobalDocumentState = DocumentState_ (ID Edit)

data WipedDocumentState =
    WipedDocumentStateView
  | WipedDocumentStateDiff
      { _wipedDocumentStateDiff          :: Edit
      , _wipedDocumentStateDiffCollapsed :: Bool
      }
  | WipedDocumentStateEdit EditToolbarProps
  deriving (Show, Eq)

globalDocumentState :: HasCallStack => DocumentState -> GlobalDocumentState
globalDocumentState = mapDocumentState (^. editID)

mkDocumentStateView :: HasCallStack => RawContent -> GlobalDocumentState
mkDocumentStateView = globalDocumentState . mkDocumentStateView_

mkDocumentStateView_ :: HasCallStack => RawContent -> DocumentState
mkDocumentStateView_ = const DocumentStateView  -- TODO: use constructor directly if it really has become this easy.

-- | The boolean 'eidChanged' indicates whether the edit currently in
-- focus has changed and the context (like the edit that we diff
-- against) does not apply any more.  If true, always switch to view
-- mode; otherwise, stay in whichever mode we are.
refreshDocumentStateView :: Bool -> GlobalDocumentState -> GlobalDocumentState
refreshDocumentStateView eidChanged = if eidChanged then const DocumentStateView else id

emptyDocumentState :: HasCallStack => GlobalDocumentState
emptyDocumentState = mkDocumentStateView emptyRawContent

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  }
  deriving (Show, Eq, Generic)

instance UnoverlapAllEq DocumentProps

emptyDocumentProps :: HasCallStack => DocumentProps
emptyDocumentProps = DocumentProps
  { _dpDocumentState     = mkDocumentStateView_ emptyRawContent
  , _dpContributionState = emptyContributionState
  }

deriveClasses
  [ ([''DocumentAction, ''DocumentState_], allClass)
  , ([''DocumentProps], [''Lens'])
  ]
