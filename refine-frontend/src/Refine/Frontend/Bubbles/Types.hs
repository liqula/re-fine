{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Refine.Frontend.Bubbles.Types where

import qualified Data.Aeson as AE
import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)
import           Data.String.Conversions

import Refine.Common.Types

data Range = Range
    { _startPoint   :: Maybe ChunkPoint
    , _endPoint     :: Maybe ChunkPoint
    , _top          :: Int
    , _bottom       :: Int
    , _scrollOffset :: Int
    }
    deriving (Show, Generic, NFData, ToJSON)

makeLenses ''Range


instance AE.FromJSON Range where
    parseJSON = AE.withObject "Range" $ \v -> Range <$>
                             v AE..:? "start" <*>
                             v AE..:? "end" <*>
                             v AE..: "top" <*>
                             v AE..: "bottom" <*>
                             v AE..: "scrollOffset"

type DeviceOffset = Int

type Selection = (Maybe Range, Maybe DeviceOffset)

-- for Overlay:
newtype CommentInputState = CommentInputState
  { _commentInputStateText     :: ST
  } deriving (Show, Typeable, Generic, NFData)

makeLenses ''CommentInputState

data CommentCategory =
    Discussion
  | Note
  deriving (Show, Typeable, Generic, NFData, ToJSON)


data BubblesState = BubblesState
  { _bsCurrentSelection       :: Selection
  , _bsCommentCategory        :: Maybe CommentCategory
  , _bsCommentIsVisible       :: Bool
  , _bsCommentEditorIsVisible :: (Bool, Maybe Range)
  } deriving (Show, Typeable, Generic, NFData, ToJSON)

makeLenses ''BubblesState
