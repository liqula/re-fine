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

import           Control.DeepSeq ( NFData )
import           Control.Lens ( makeLenses )
import           Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import           GHC.Generics ( Generic )
import           GHCJS.Types ( JSVal )

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Types
import           Refine.Prelude.TH ( makeRefineType )

data DocumentAction =
    UpdateEditorState EditorState
  deriving (Show, Generic)


data DocumentState = DocumentState
  { _dsEditMode           :: Maybe EditKind
  , _dsEditorState        :: Maybe EditorState
  } deriving (Generic, Show)

emptyDocumentState :: DocumentState
emptyDocumentState = DocumentState Nothing Nothing


data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  , _dpToolbarStatus     :: ToolbarExtensionStatus
  , _dpVDocVersion       :: VDocVersion 'HTMLWithMarks
  }

newtype EditorState = EditorState { _unEditorState :: JSVal }
  deriving (Generic, NFData)

instance ToJSON EditorState where
  toJSON _ = toJSON ("EditorState" :: String)

instance FromJSON EditorState where
  parseJSON _ = fail "cannot parse EditorState"

instance Show EditorState where
  show _ = "EditorState"

makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeLenses ''DocumentProps
