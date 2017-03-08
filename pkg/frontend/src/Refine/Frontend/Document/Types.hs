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

import GHC.Generics (Generic)
import Control.Lens (makeLenses)

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.Types
import           Refine.Prelude.TH (makeRefineType)

data DocumentAction =
    DocumentAction -- no concrete actions yet
  deriving (Show, Generic)


newtype DocumentState = DocumentState
  { _dsEditMode           :: Maybe EditKind
  } deriving (Show, Generic)

emptyDocumentState :: DocumentState
emptyDocumentState = DocumentState Nothing


data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  , _dpToolbarStatus     :: ToolbarExtensionStatus
  , _dpVDocVersion       :: VDocVersion 'HTMLWithMarks
  }


makeRefineType ''DocumentAction
makeRefineType ''DocumentState
makeLenses ''DocumentProps
