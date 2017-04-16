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
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Header.Types
import           Refine.Prelude.TH (makeRefineType)


data DocumentAction =
    DocumentEditUpdate DocumentEditState
  | DocumentEditSave
  deriving (Show, Eq, Generic)

data DocumentState =
    DocumentStateView
  | DocumentStateEdit { _documentStateEdit :: DocumentEditState }
  deriving (Show, Eq, Generic)

data DocumentEditState = DocumentEditState
  { _documentEditStateKind      :: EditKind
  , _documentEditStateVal       :: EditorState
  }
  deriving (Show, Eq, Generic)

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  , _dpToolbarStatus     :: ToolbarExtensionStatus
  , _dpVDocVersion       :: VDocVersion 'HTMLWithMarks
  }
  deriving (Show, Eq, Generic)

newtype EditorProps = EditorProps
  { _ewpEditorState :: DocumentEditState
  }
  deriving (Eq)

instance UnoverlapAllEq DocumentProps
instance UnoverlapAllEq EditorProps

makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeRefineType ''DocumentEditState
makeLenses ''DocumentProps
makeLenses ''EditorProps
