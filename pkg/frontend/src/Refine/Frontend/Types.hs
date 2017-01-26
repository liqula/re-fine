{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Refine.Frontend.Types where

import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Int
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, encode, toJSON, Value, (.=), object)
import           Data.String.Conversions

import Refine.Common.Types

import Refine.Frontend.Bubbles.Types

newtype MarkPositions = MarkPositions { _unMarkPositions :: M.Map Int64 Int }
  deriving (Eq, Show, Typeable, Generic, NFData)

mapToValue :: (ToJSON k, ToJSON v) => M.Map k v -> Value
mapToValue = object . fmap (\(k,v) -> (cs . encode) k .= v) . M.toList

instance ToJSON MarkPositions where
  toJSON = toJSON . mapToValue . _unMarkPositions

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Typeable, Generic, NFData, ToJSON)

data GlobalState = GlobalState
  { _gsVDoc                   :: Maybe CompositeVDoc
  , _gsVDocList               :: Maybe [ID VDoc]
  , _gsHeaderHeight           :: Int
  , _gsMarkPositions          :: MarkPositions
  , _gsWindowSize             :: WindowSize
  , _gsBubblesState           :: BubblesState
  } deriving (Show, Typeable, Generic, NFData, ToJSON)

makeLenses ''GlobalState

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState Nothing Nothing 0 (MarkPositions M.empty) Desktop emptyBubblesState

data RefineAction = LoadDocumentList
                  | LoadedDocumentList [ID VDoc]
                  | LoadDocument (ID VDoc)
                  | OpenDocument CompositeVDoc
                  | AddDemoDocument
                  | AddHeaderHeight Int
                  | AddMarkPosition Int64 Int
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
  deriving (Show, Typeable, Generic, NFData, ToJSON)
