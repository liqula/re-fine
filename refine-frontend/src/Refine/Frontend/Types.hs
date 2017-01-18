{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Refine.Frontend.Types where

import qualified Data.Aeson as AE
import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, encode, toJSON, Value, (.=), object)
import           Data.String.Conversions

import Refine.Common.Types
import Refine.Common.Rest


newtype MarkPositions = MarkPositions { _unMarkPositions :: M.Map String Int }
  deriving (Eq, Show, Typeable, Generic, NFData)

mapToValue :: (ToJSON k, ToJSON v) => M.Map k v -> Value
mapToValue = object . fmap (\(k,v) -> (cs . encode) k .= v) . M.toList

instance ToJSON MarkPositions where
  toJSON = toJSON . mapToValue . _unMarkPositions

data WindowSize = Desktop | Tablet | Mobile
  deriving (Show, Typeable, Generic, NFData, ToJSON)

type DeviceOffset = Int

data Range = Range
    { _startUid     :: Maybe Int
    , _startOffset  :: Int -- TODO make this a Maybe
    , _endUid       :: Maybe Int
    , _endOffset    :: Int -- TODO make this a Maybe
    , _top          :: Int
    , _bottom       :: Int
    , _scrollOffset :: Int
    }
    deriving (Show, Generic, NFData, ToJSON)

makeLenses ''Range

instance AE.FromJSON Range where
    parseJSON (AE.Object v) = Range <$>
                             v AE..:? "ns" <*>
                             v AE..: "os" <*>
                             v AE..:? "ne" <*>
                             v AE..: "oe" <*>
                             v AE..: "top" <*>
                             v AE..: "bottom" <*>
                             v AE..: "scrollOffset"
    parseJSON _          = error "not an object... what can we do?" -- TODO empty

data RefineState = RefineState
  { _rsVDoc             :: Maybe CompositeVDoc
  , _rsVDocList         :: Maybe [ID VDoc]
  , _rsHeaderHeight     :: Int
  , _rsMarkPositions    :: MarkPositions
  , _rsWindowSize       :: WindowSize
  , _rsCurrentSelection :: (Maybe Range, Maybe DeviceOffset)
  } deriving (Show, Typeable, Generic, NFData, ToJSON)

makeLenses ''RefineState

data RefineAction = LoadDocumentList
                  | LoadedDocumentList [ID VDoc]
                  | LoadDocument (ID VDoc)
                  | OpenDocument CompositeVDoc
                  | AddDemoDocument
                  | AddHeaderHeight Int
                  | AddMarkPosition String Int
                  | SetWindowSize WindowSize
                  | SetSelection DeviceOffset
                  | SubmitPatch
                  | SaveSelect Text Text
  deriving (Show, Typeable, Generic, NFData)
