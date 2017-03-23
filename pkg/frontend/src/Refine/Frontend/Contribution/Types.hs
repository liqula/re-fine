{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
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


module Refine.Frontend.Contribution.Types where

import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Aeson (toJSON, parseJSON, object, (.=), (.:), (.:?), withObject)
import           Data.Aeson.Types (FromJSON, ToJSON, Value, Parser)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import           Data.String.Conversions
import           GHC.Generics (Generic)
import           React.Flux (PropertyOrHandler, ViewEventHandler)
import           Text.Read (readMaybe)

import Refine.Common.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.UtilityWidgets
import Refine.Prelude.TH (makeRefineType)


data Range = Range
    { _rangeStartPoint   :: Maybe ChunkPoint
    , _rangeEndPoint     :: Maybe ChunkPoint
    , _rangeTopOffset    :: OffsetFromViewportTop
    , _rangeBottomOffset :: OffsetFromViewportTop
    , _rangeScrollOffset :: ScrollOffsetOfViewport
    }
    deriving (Show, Generic, NFData)

makeLenses ''Range

instance FromJSON Range where
    parseJSON = withObject "Range" $ \v -> Range <$>
                             v .:? "start" <*>
                             v .:? "end" <*>
                             v .: "top" <*>
                             v .: "bottom" <*>
                             v .: "scrollOffset"

instance ToJSON Range where
    toJSON (Range sp ep t b s) = object
      [ "start"        .= sp
      , "end"          .= ep
      , "top"          .= t
      , "bottom"       .= b
      , "scrollOffset" .= s
      ]

data Selection =
    NothingSelected
  | NothingSelectedButUpdateTriggered OffsetFromDocumentTop -- TODO when can this happen?
  | RangeSelected Range OffsetFromDocumentTop
  deriving (Show, Generic)

-- for Overlay:
newtype CommentInputState = CommentInputState
  { _commentInputStateText     :: ST
  } deriving (Show, Generic)

data CommentCategory =
    Discussion
  | Note
  deriving (Show, Eq, Generic)

-- for marks:
newtype MarkPositions = MarkPositions { _unMarkPositions :: M.Map ContributionID MarkPosition }
  deriving (Eq, Show, Generic)

data MarkPosition = MarkPosition
  { _markPositionTop    :: OffsetFromDocumentTop
  , _markPositionBottom :: OffsetFromDocumentTop
  }
  deriving (Eq, Show, Generic)

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

data ContributionEditorData =
    EditorIsVisible (Maybe Range)
  | EditorIsHidden
  deriving (Show, Generic)


data ContributionAction =
    UpdateSelection Selection ToolbarExtensionStatus
  | ShowContributionDialog ContributionID
  | HideCommentOverlay
  | ShowCommentEditor (Maybe Range)
  | HideCommentEditor
  | SetCommentCategory CommentCategory
  | SubmitComment ST (Maybe CommentCategory) (Maybe Range)
  | SubmitEdit
  | AddMarkPosition ContributionID MarkPosition
  | HighlightMarkAndBubble ContributionID
  | UnhighlightMarkAndBubble
  deriving (Show, Generic)


data ContributionState = ContributionState
  { _csCurrentSelection         :: Selection
  , _csCommentCategory          :: Maybe CommentCategory
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csCommentEditorIsVisible   :: ContributionEditorData
  , _csHighlightedMarkAndBubble :: Maybe ContributionID
  , _csMarkPositions            :: MarkPositions
  } deriving (Show, Generic)


emptyContributionState :: ContributionState
emptyContributionState = ContributionState NothingSelected Nothing Nothing EditorIsHidden Nothing (MarkPositions M.empty)


makeRefineType ''CommentInputState
makeRefineType ''CommentCategory
makeRefineType ''Selection
makeRefineType ''ContributionEditorData
makeRefineType ''ContributionAction
makeRefineType ''ContributionState

makeRefineType ''MarkPosition

deriving instance NFData MarkPositions

instance ToJSON MarkPositions where
  toJSON = mapToValue . _unMarkPositions

instance FromJSON MarkPositions where
  parseJSON = fmap MarkPositions . mapFromValue


data MarkProps = MarkProps
  { _markPropsViewEventHandlers     :: [PropertyOrHandler ViewEventHandler]
  , _markPropsContributionID        :: ContributionID
  , _markPropsHighlightedMark       :: Maybe ContributionID
  , _markPropsDisplayedContribution :: Maybe ContributionID
  }

makeLenses ''MarkProps

data BubbleProps = BubbleProps
  { _bubblePropsDataContribId     :: ContributionID
  , _bubblePropsIconSide          :: String  -- FIXME: either "left" or "right", make this a custom boolean!
  , _bubblePropsIconStyle         :: IconDescription
  , _bubblePropsMarkPosition      :: Maybe MarkPosition
  , _bubblePropsHighlightedBubble :: Maybe ContributionID
  , _bubblePropsClickHandler      :: ClickHandler
  , _bubblePropsScreenState       :: ScreenState
  }

makeLenses ''BubbleProps

data SpecialBubbleProps = SpecialBubbleProps
  { _specialBubblePropsContributionId    :: ContributionID
  , _specialBubblePropsMarkPosition      :: Maybe MarkPosition
  , _specialBubblePropsHighlightedBubble :: Maybe ContributionID
  , _specialBubblePropsScreenState       :: ScreenState
  }

makeLenses ''SpecialBubbleProps
