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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Types where

import           Control.DeepSeq
import qualified Data.HashMap.Strict as HashMap
import           Data.Int
import           Data.Text (Text)
import           Data.String.Conversions
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import           Data.Aeson (toJSON, parseJSON, object, (.=), withObject)
import           Data.Aeson.Types (FromJSON, ToJSON, Value, Parser)
import           Text.Read (readMaybe)

import Refine.Common.Types

import Refine.Frontend.Bubbles.Types
import Refine.Frontend.Screen.Types
import Refine.Prelude.TH (makeRefineType)


newtype MarkPositions = MarkPositions { _unMarkPositions :: M.Map Int64 (Int, Int) }
  deriving (Eq, Show, Generic, NFData)

-- | TODO: we have orphan instances for maps in Refine.Common.Orphans.  we should:
-- (1) move this function there;
-- (2) implement the orphan instances in terms of this function, not via lists;
-- (3) same for @mapFromValue@.
-- (4) rename to @map{From,To}JSON@.
mapToValue :: (Show k, ToJSON v) => M.Map k v -> Value
mapToValue = object . fmap (\(k, v) -> (cs . show) k .= v) . M.toList

mapFromValue :: (Ord k, Read k, FromJSON v) => Value -> Parser (M.Map k v)
mapFromValue = withObject "MarkPositions"
  $ fmap M.fromList
  . mapM (\(k, v) -> (,) <$> maybe (fail "could not parse key.") pure (readMaybe (cs k))
                         <*> parseJSON v)
  . HashMap.toList

instance ToJSON MarkPositions where
  toJSON = mapToValue . _unMarkPositions

instance FromJSON MarkPositions where
  parseJSON = fmap MarkPositions . mapFromValue

data GlobalState = GlobalState
  { _gsVDoc                   :: Maybe CompositeVDoc
  , _gsVDocList               :: Maybe [ID VDoc]
  , _gsMarkPositions          :: MarkPositions
  , _gsBubblesState           :: BubblesState
  , _gsScreenState            :: ScreenState
  } deriving (Show, Generic)

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState Nothing Nothing (MarkPositions M.empty) emptyBubblesState emptyScreenState

data RefineAction = LoadDocumentList
                  | LoadedDocumentList [ID VDoc]
                  | LoadDocument (ID VDoc)
                  | OpenDocument CompositeVDoc
                  | AddDemoDocument
                  | AddHeaderHeight Int
                  | AddMarkPosition Int64 OffsetFromViewportTop ScrollOffsetOfViewport
                  | SetWindowSize WindowSize
                  -- Bubble Actions:
                  | UpdateSelection Selection
                  | ClearSelection
                  | ShowComment
                  | HideComment
                  | ShowCommentEditor (Maybe Range)
                  | HideCommentEditor
                  | SetCommentCategory CommentCategory
                  | SubmitComment ST (Maybe CommentCategory) (Maybe Range)
                  -- ...
                  | AddDiscussion CompositeDiscussion
                  | AddNote Note
                  | SubmitEdit
                  | SaveSelect Text Text
                  -- Actions that will be transformed because they need IO:
                  | TriggerUpdateSelection DeviceOffset
  deriving (Show, Generic)

makeRefineType ''GlobalState
makeRefineType ''RefineAction
