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
  | DocumentSave
  | DocumentToggleBold
  | DocumentToggleItalic
  deriving (Show, Eq, Generic)

-- | FIXME: 'documentStateEditKind' will fan out into a 'EditInfo' record containing 'EditKind' and
-- other stuff, see #233.
data DocumentState =
    DocumentStateView
      { _documentStateContent  :: RawContent  -- ^ in read-only mode, change to the content is
                                              -- driven by haskell, so we keep the haskell
                                              -- representation around.
      , _documentStateVal      :: EditorState
      }
  | DocumentStateEdit
      { _documentStateVal      :: EditorState
      , _documentStateEditKind :: EditKind
      }
  deriving (Show, Eq, Generic)

mkDocumentStateView :: RawContent -> DocumentState
mkDocumentStateView c = DocumentStateView c' e
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

makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeLenses ''DocumentProps
