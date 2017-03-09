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

import           Control.Lens ( makeLenses )
import           GHC.Generics ( Generic )
import           GHCJS.Types ( JSVal )

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Types
import           Refine.Prelude.Aeson (NoJSONRep(..))
import           Refine.Prelude.TH ( makeRefineType )


newtype DocumentAction =
    UpdateEditorState EditorState
  deriving (Show, Generic)


data DocumentState = DocumentStateView | DocumentStateEdit EditorState
  deriving (Generic, Show)

data EditorState = EditorState
  { _editorStateKind :: EditKind
  , _editorStateVal  :: NoJSONRep JSVal
  }
  deriving (Generic, Show)


data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  , _dpToolbarStatus     :: ToolbarExtensionStatus
  , _dpVDocVersion       :: VDocVersion 'HTMLWithMarks
  }


makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeRefineType ''EditorState
makeLenses ''DocumentProps
