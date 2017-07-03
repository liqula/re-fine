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
import           Refine.Frontend.Document.FFI


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

data DocumentState_ rawcontent edit =
    DocumentStateView
      { _documentStateVal      :: EditorState
      , _documentStateContent  :: rawcontent
      }
  | DocumentStateDiff
      { _documentStateVal           :: EditorState
      , _documentStateContent       :: rawcontent
      , _documentStateDiff          :: edit
      , _documentStateDiffCollapsed :: Bool
      }
  | DocumentStateEdit
      { _documentStateVal      :: EditorState
      , _documentStateEditInfo :: EditInfo (Maybe EditKind)  -- ^ 'editInput_' dialog state lives here between close / re-open.
      }
  deriving (Show, Eq, Generic, Functor)

mapDocumentState :: (a -> a') -> (b -> b') -> DocumentState_ a b -> DocumentState_ a' b'
mapDocumentState f g = \case
  DocumentStateView x a -> DocumentStateView x (f a)
  DocumentStateDiff x a e y -> DocumentStateDiff x (f a) (g e) y
  DocumentStateEdit x y -> DocumentStateEdit x y

-- | The document state variant for 'DocumentProps'.
type DocumentState = DocumentState_ RawContent Edit

-- | The document state for 'GlobalState'.
type GlobalDocumentState = DocumentState_ () (ID Edit)

data WipedDocumentState =
    WipedDocumentStateView
  | WipedDocumentStateDiff
      { _wipedDocumentStateDiff          :: Edit
      }
  | WipedDocumentStateEdit EditToolbarProps
  deriving (Show, Eq)

globalDocumentState :: HasCallStack => DocumentState -> GlobalDocumentState
globalDocumentState = mapDocumentState (const ()) (^. editID)

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
refreshDocumentStateView :: Bool -> RawContent -> GlobalDocumentState -> GlobalDocumentState
refreshDocumentStateView eidChanged c = if eidChanged then viewMode else sameMode
  where
    viewMode _ = DocumentStateView e ()

    sameMode = \case
      DocumentStateView _ _                -> DocumentStateView e ()
      DocumentStateDiff _ _ edit collapsed -> DocumentStateDiff e () edit collapsed
      DocumentStateEdit _ kind             -> DocumentStateEdit e kind

    e  = createWithContent $ convertFromRaw c

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
