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

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI


data DocumentAction =
    UpdateEditorState EditorState
  | DocumentUpdateEditInfo (EditInfo (Maybe EditKind))
  | RequestDocumentSave  -- ^ FIXME: use the 'AfterAjax' trick here?
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

data DocumentState_ editable{-() or Bool-} rawcontent edit discussion =
    DocumentStateView
      { _documentStateVal      :: EditorState
      , _documentStateContent  :: rawcontent
      }
  | DocumentStateDiff
      { _documentStateDiffIndex     :: EditIndex
      , _documentStateVal           :: EditorState
      , _documentStateContent       :: rawcontent
      , _documentStateDiff          :: edit
      , _documentStateDiffCollapsed :: Bool
      , _documentStateDiffEditable  :: editable -- derived in global state, so it is () there
      }
  | DocumentStateEdit
      { _documentStateVal      :: EditorState
      , _documentStateEditInfo :: EditInfo (Maybe EditKind)  -- ^ 'editInput_' dialog state lives here between close / re-open.
      , _documentBaseEdit      :: Maybe (ID Edit)  -- ^ Just if we are updating and edit
      }
  | DocumentStateDiscussion
      { _documentDiscussion    :: discussion
      }
  deriving (Show, Eq, Generic, Functor)

mapDocumentState :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> DocumentState_ a b c d -> DocumentState_ a' b' c' d'
mapDocumentState fa fb fc fd = \case
  DocumentStateView x a -> DocumentStateView x (fb a)
  DocumentStateDiff i x a e y ed -> DocumentStateDiff i x (fb a) (fc e) y (fa ed)
  DocumentStateEdit x y be -> DocumentStateEdit x y be
  DocumentStateDiscussion d -> DocumentStateDiscussion (fd d)

-- | The document state variant for 'DocumentProps'.
type DocumentState = DocumentState_ Bool RawContent Edit Discussion

-- | The document state for 'GlobalState'.
type GlobalDocumentState = DocumentState_ () () (ID Edit) (ID Discussion)

data WipedDocumentState =
    WipedDocumentStateView
  | WipedDocumentStateDiff
      { _wipedDocumentStateDiffIndex     :: EditIndex
      , _wipedDocumentStateDiff          :: Edit
      , _wipedDocumentStateDiffCollapsed :: Bool
      , _wipedDocumentStateDiffEditable  :: Bool
      }
  | WipedDocumentStateEdit EditToolbarProps
  | WipedDocumentStateDiscussion
  deriving (Show, Eq)

globalDocumentState :: HasCallStack => DocumentState -> GlobalDocumentState
globalDocumentState = mapDocumentState (const ()) (const ()) (^. editID) (^. discussionID)

mkDocumentStateView :: HasCallStack => RawContent -> GlobalDocumentState
mkDocumentStateView = globalDocumentState . mkDocumentStateView_

mkDocumentStateView_ :: HasCallStack => RawContent -> DocumentState
mkDocumentStateView_ c = DocumentStateView e c'
  where
    e  = createWithContent $ convertFromRaw c
    c' = convertToRaw $ getCurrentContent e

-- | The boolean 'eidChanged' indicates whether the edit currently in
-- focus has changed and the context (like the edit that we diff
-- against) does not apply any more.  If true, always switch to view
-- mode; otherwise, stay in whichever mode we are.
refreshDocumentStateView :: Edit -> Bool -> RawContent -> GlobalDocumentState -> GlobalDocumentState
refreshDocumentStateView ed eidChanged c = if eidChanged then viewMode else sameMode
  where
    viewMode _ = DocumentStateView e ()

    sameMode = \case
      DocumentStateView _ _                  -> DocumentStateView e ()
      DocumentStateDiff _ _ _ edit collapsed editable -> DocumentStateDiff (mkEditIndex ed edit) e () edit collapsed editable
      DocumentStateEdit _ kind Nothing       -> DocumentStateEdit e kind Nothing
      dst@(DocumentStateEdit _ _ Just{})     -> dst -- FIXME: not sure about this
      DocumentStateDiscussion did            -> DocumentStateDiscussion did

    e  = createWithContent $ convertFromRaw c

emptyDocumentState :: HasCallStack => GlobalDocumentState
emptyDocumentState = mkDocumentStateView emptyRawContent

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  }
  deriving (Show, Eq, Generic)

emptyDocumentProps :: HasCallStack => DocumentProps
emptyDocumentProps = DocumentProps
  { _dpDocumentState     = mkDocumentStateView_ emptyRawContent
  , _dpContributionState = emptyContributionState
  }

deriveClasses
  [ ([''DocumentAction, ''DocumentState_], allClass)
  , ([''DocumentProps, ''WipedDocumentState], [''Lens'])
  ]
