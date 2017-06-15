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

import           Control.Lens (makeLenses)
import           GHC.Generics (Generic)
import           React.Flux (UnoverlapAllEq)

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Header.Types
import           Refine.Prelude.TH (makeRefineType)


data DocumentAction =
    DocumentUpdate DocumentState
  | DocumentUpdateEditKind EditKind
  | RequestDocumentSave
  | DocumentSave ST
  | DocumentCancelSave
  | DocumentToggleStyle Style
  | DocumentToggleBlockType BlockType
  | DocumentToggleLink -- spawns either DocumentRemoveLink or OpenEditToolbarLinkEditor
  | DocumentRemoveLink
  | DocumentCreateLink ST
  | ToggleCollapseDiff
  deriving (Show, Eq, Generic)

data DocumentState =
    DocumentStateView
      { _documentStateVal      :: EditorState
      , _documentStateContent  :: RawContent  -- ^ in read-only mode, change to the content is
                                              -- driven by haskell, so we keep the haskell
                                              -- representation around.
      }
  | DocumentStateDiff
      { _documentStateVal           :: EditorState
      , _documentStateContent       :: RawContent
      , _documentStateDiff          :: Edit
      , _documentStateDiffCollapsed :: Bool
      }
  | DocumentStateEdit
      { _documentStateVal      :: EditorState
      , _documentStateEditKind :: EditKind
      }
  deriving (Show, Eq, Generic)

mkDocumentStateView :: RawContent -> DocumentState
mkDocumentStateView c = DocumentStateView e c'
  where
    e  = createWithContent $ convertFromRaw c
    c' = convertToRaw $ getCurrentContent e

emptyDocumentState :: DocumentState
emptyDocumentState = mkDocumentStateView emptyRawContent

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  , _dpToolbarStatus     :: ToolbarExtensionStatus
  }
  deriving (Show, Eq, Generic)

instance UnoverlapAllEq DocumentProps

emptyDocumentProps :: DocumentProps
emptyDocumentProps = DocumentProps
  { _dpDocumentState     = emptyDocumentState
  , _dpContributionState = emptyContributionState
  , _dpToolbarStatus     = ToolbarExtensionClosed
  }

makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeLenses ''DocumentProps
