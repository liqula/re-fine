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
import           GHCJS.Types (JSVal)

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Util (js_eq)
import           Refine.Prelude.Aeson (NoJSONRep(..))
import           Refine.Prelude.TH (makeRefineType)


data DocumentAction =
    TriggerDocumentEditStart EditorState
  | DocumentEditStart EditorState
  | DocumentEditSave
  deriving (Show, Eq, Generic)

data DocumentState =
    DocumentStateView
  | DocumentStateEdit { _documentStateEdit :: EditorState }
  deriving (Show, Eq, Generic)

data EditorState = EditorState
  { _editorStateKind      :: EditKind
  , _editorStateVal       :: NoJSONRep JSVal
  , _editorStateSelection :: Maybe Range
  }
  deriving (Show, Generic)

instance Eq EditorState where
  EditorState k (NoJSONRep js) mr == EditorState k' (NoJSONRep js') mr'
      = k == k'
     && js_eq js js'  -- (not too confident about this one...)
     && mr == mr'

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  , _dpToolbarStatus     :: ToolbarExtensionStatus
  , _dpVDocVersion       :: VDocVersion 'HTMLWithMarks
  }
  deriving (Show, Eq, Generic)

newtype EditorWrapperProps = EditorWrapperProps
  { _ewpEditorState :: EditorState
  }
  deriving (Eq)

makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeRefineType ''EditorState
makeLenses ''DocumentProps
makeLenses ''EditorWrapperProps
