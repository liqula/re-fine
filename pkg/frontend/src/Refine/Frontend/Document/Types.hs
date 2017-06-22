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
import           Refine.Prelude.TH (makeRefineType)


data DocumentAction =
    DocumentUpdate DocumentState
  | DocumentUpdateEditKind EditKind
  | RequestDocumentSave
  | DocumentSave ST
  | DocumentCancelSave
  | DocumentToggleStyle Style
  | DocumentToggleBlockType BlockType
  | DocumentRemoveLink
  | DocumentCreateLink ST
  | ToggleCollapseDiff
  | DocumentUndo
  | DocumentRedo
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

mkDocumentStateView :: HasCallStack => RawContent -> DocumentState
mkDocumentStateView c = DocumentStateView e c'
  where
    e  = createWithContent $ convertFromRaw c
    c' = convertToRaw $ getCurrentContent e

-- | The boolean 'eidChanged' indicates whether the edit currently in
-- focus has changed and the context (like the edit that we diff
-- against) does not apply any more.  If true, always switch to view
-- mode; otherwise, stay in whichever mode we are.
refreshDocumentStateView :: Bool -> RawContent -> DocumentState -> DocumentState
refreshDocumentStateView eidChanged c = if eidChanged then viewMode else sameMode
  where
    viewMode _ = DocumentStateView e c

    sameMode = \case
      DocumentStateView _ _                -> DocumentStateView e c
      DocumentStateDiff _ _ edit collapsed -> DocumentStateDiff e c edit collapsed
      DocumentStateEdit _ kind             -> DocumentStateEdit e kind

    e  = createWithContent $ convertFromRaw c

emptyDocumentState :: HasCallStack => DocumentState
emptyDocumentState = mkDocumentStateView emptyRawContent

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  }
  deriving (Show, Eq, Generic)

instance UnoverlapAllEq DocumentProps

emptyDocumentProps :: HasCallStack => DocumentProps
emptyDocumentProps = DocumentProps
  { _dpDocumentState     = emptyDocumentState
  , _dpContributionState = emptyContributionState
  }

makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeLenses ''DocumentProps
