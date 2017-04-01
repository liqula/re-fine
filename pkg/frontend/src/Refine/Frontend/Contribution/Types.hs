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
import           Data.Maybe (catMaybes)
import           Data.String.Conversions
import           GHC.Generics (Generic)
import           Text.HTML.Parser (Attr)
import           Text.Read (readMaybe)

import Refine.Common.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Prelude.TH (makeRefineType)
import Refine.Prelude.Aeson ((.=?))


data Range = Range
    { _rangeStartPoint   :: Maybe ChunkPoint
    , _rangeEndPoint     :: Maybe ChunkPoint
    , _rangeTopOffset    :: OffsetFromViewportTop
    , _rangeBottomOffset :: OffsetFromViewportTop
    , _rangeScrollOffset :: ScrollOffsetOfViewport
    }
    deriving (Show, Eq, Generic, NFData)

makeLenses ''Range

instance FromJSON Range where
    parseJSON = withObject "Range" $ \v -> Range <$>
                             v .:? "start" <*>
                             v .:? "end" <*>
                             v .: "top" <*>
                             v .: "bottom" <*>
                             v .: "scrollOffset"

instance ToJSON Range where
    toJSON (Range sp ep t b s) = object $
      catMaybes [ "start" .=? sp
                , "end"   .=? ep
                ]
      <> [ "top"          .= t
         , "bottom"       .= b
         , "scrollOffset" .= s
         ]

data Selection =
    NothingSelected
  | NothingSelectedButUpdateTriggered OffsetFromDocumentTop  -- TODO when can this happen?
  | RangeSelected Range OffsetFromDocumentTop
  deriving (Show, Eq, Generic)

-- | for overlay
newtype CommentInputState = CommentInputState
  { _commentInputStateText :: ST
  } deriving (Show, Eq, Generic)

data CommentKind =
    CommentKindNote
  -- | CommentKindQuestion
  | CommentKindDiscussion
  deriving (Show, Eq, Generic)

-- for marks:
newtype MarkPositions = MarkPositions { _unMarkPositions :: M.Map ContributionID MarkPosition }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)


data ContributionAction =
    UpdateSelection Selection ToolbarExtensionStatus
  | ShowContributionDialog ContributionID
  | HideCommentOverlay
  | ShowCommentEditor (Maybe Range)
  | HideCommentEditor
  | SetCommentKind CommentKind
  | SubmitComment ST (Maybe CommentKind) (Maybe Range)
  | SubmitEdit
  | AddMarkPosition ContributionID MarkPosition
  | HighlightMarkAndBubble ContributionID
  | UnhighlightMarkAndBubble
  deriving (Show, Eq, Generic)


data ContributionState = ContributionState
  { _csCurrentSelection         :: Selection
  , _csCommentKind              :: Maybe CommentKind
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csCommentEditorIsVisible   :: ContributionEditorData
  , _csHighlightedMarkAndBubble :: Maybe ContributionID
  , _csMarkPositions            :: MarkPositions
  } deriving (Show, Eq, Generic)


emptyContributionState :: ContributionState
emptyContributionState = ContributionState NothingSelected Nothing Nothing EditorIsHidden Nothing (MarkPositions M.empty)


makeRefineType ''CommentInputState
makeRefineType ''CommentKind
makeRefineType ''Selection
makeRefineType ''ContributionEditorData
makeRefineType ''ContributionAction
makeRefineType ''ContributionState

makeRefineType ''MarkPosition

makeLenses ''MarkPositions

deriving instance NFData MarkPositions

instance ToJSON MarkPositions where
  toJSON = mapToValue . _unMarkPositions

instance FromJSON MarkPositions where
  parseJSON = fmap MarkPositions . mapFromValue


data MarkProps = MarkProps
  { _markPropsAttrs                 :: [Attr]
  , _markPropsContributionID        :: ContributionID
  , _markPropsHighlightedMark       :: Maybe ContributionID
  , _markPropsDisplayedContribution :: Maybe ContributionID
  }
  deriving (Eq)

makeLenses ''MarkProps

data BubbleProps = BubbleProps
  { _bubblePropsDataContribId     :: ContributionID
  , _bubblePropsIconSide          :: String  -- FIXME: either "left" or "right", make this a custom boolean!
  , _bubblePropsIconStyle         :: IconDescription
  , _bubblePropsMarkPosition      :: Maybe MarkPosition
  , _bubblePropsHighlightedBubble :: Maybe ContributionID
  , _bubblePropsClickActions      :: [ContributionAction]
  , _bubblePropsScreenState       :: ScreenState
  }
  deriving (Eq)

makeLenses ''BubbleProps

data SpecialBubbleProps = SpecialBubbleProps
  { _specialBubblePropsContributionId    :: ContributionID
  , _specialBubblePropsMarkPosition      :: Maybe MarkPosition
  , _specialBubblePropsHighlightedBubble :: Maybe ContributionID
  , _specialBubblePropsScreenState       :: ScreenState
  }
  deriving (Eq)

makeLenses ''SpecialBubbleProps
