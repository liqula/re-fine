{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Frontend.Prelude

import           Control.DeepSeq
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map

import Refine.Common.Types hiding (Style)
import Refine.Frontend.Screen.Types
import Refine.Frontend.Style
import Refine.Frontend.Types
import Refine.Prelude.TH (makeRefineType)


newtype CommentInputState = CommentInputState
  { _commentInputStateText :: ST
  } deriving (Show, Eq, Generic)


newtype MarkPositions = MarkPositions { _markPositionsMap :: Map.Map ContributionID MarkPosition }
  deriving (Show, Eq, Generic)

instance Monoid MarkPositions where
  mempty = MarkPositions mempty
  mappend (MarkPositions m) (MarkPositions m') = MarkPositions (m <> m')

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
mapToValue :: (Show k, ToJSON v) => Map.Map k v -> Value
mapToValue = object . fmap (\(k, v) -> (cs . show) k .:= v) . Map.toList

mapFromValue :: (Ord k, Read k, FromJSON v) => Value -> Parser (Map.Map k v)
mapFromValue = withObject "MarkPositions"
  $ fmap Map.fromList
  . mapM (\(k, v) -> (,) <$> maybe (fail "could not parse key.") pure (readMaybe (cs k))
                         <*> parseJSON v)
  . HashMap.toList


-- * Contribution

-- | TODO: give record selectors to all fields.
data ContributionAction =
    RequestSetRange -- TODO: move this to 'DocumentAction'
  | SetRange Range  -- TODO: move this to 'DocumentAction'
  | ClearRange      -- TODO: move this to 'DocumentAction'
  | ShowContributionDialog ContributionID
  | HideCommentOverlay
  | ShowCommentEditor
  | HideCommentEditor
  | SetCommentKind CommentKind
  | SubmitComment ST (Maybe CommentKind)
  | RequestSetMarkPositions
  | SetMarkPositions [(ContributionID, MarkPosition)]  -- ^ see 'MarkPositions'
  | HighlightMarkAndBubble ContributionID
  | UnhighlightMarkAndBubble
  deriving (Show, Eq, Generic)


data ContributionState = ContributionState
  { _csCurrentRange             :: Maybe Range
  , _csCommentKind              :: Maybe CommentKind
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csActiveDialog             :: Maybe ActiveDialog
  , _csHighlightedMarkAndBubble :: Maybe ContributionID
  , _csQuickCreateShowState     :: QuickCreateShowState
  , _csMarkPositions            :: MarkPositions
  } deriving (Show, Eq, Generic)

data CommentKind =
    CommentKindNote
  -- | CommentKindQuestion
  | CommentKindDiscussion
  deriving (Show, Eq, Generic)

data ActiveDialog = ActiveDialogComment | ActiveDialogEdit
  deriving (Show, Eq, Generic)

emptyContributionState :: ContributionState
emptyContributionState = ContributionState
  { _csCurrentRange             = Nothing
  , _csCommentKind              = Nothing
  , _csDisplayedContributionID  = Nothing
  , _csActiveDialog             = Nothing
  , _csHighlightedMarkAndBubble = Nothing
  , _csQuickCreateShowState     = QuickCreateNotShown
  , _csMarkPositions            = mempty
  }


-- * Bubble

data BubbleProps = BubbleProps
  { _bubblePropsContributionId    :: ContributionID
  , _bubblePropsIconSide          :: String  -- FIXME: either "left" or "right", make this a custom boolean!
  , _bubblePropsIconStyle         :: IconDescription
  , _bubblePropsMarkPosition      :: Maybe MarkPosition
  , _bubblePropsHighlightedBubble :: Maybe ContributionID
  , _bubblePropsClickActions      :: [ContributionAction]
  , _bubblePropsScreenState       :: ScreenState
  }
  deriving (Eq)

instance UnoverlapAllEq BubbleProps

data SpecialBubbleProps = SpecialBubbleProps
  { _specialBubblePropsContributionId    :: ContributionID
  , _specialBubblePropsMarkPosition      :: Maybe MarkPosition
  , _specialBubblePropsHighlightedBubble :: Maybe ContributionID
  , _specialBubblePropsScreenState       :: ScreenState
  }
  deriving (Eq)

instance UnoverlapAllEq SpecialBubbleProps


-- * QuickCreate

data QuickCreateProps = QuickCreateProps
  { _quickCreateSide        :: QuickCreateSide
  , _quickCreateShowState   :: QuickCreateShowState
  , _quickCreateRange       :: Maybe Range
  , _quickCreateScreenState :: ScreenState
  }
  deriving (Show, Eq)

data QuickCreateSide = QuickCreateComment | QuickCreateEdit
  deriving (Show, Eq, Generic)

renderQuickCreateSide :: QuickCreateSide -> JSString
renderQuickCreateSide QuickCreateComment = "o-add-annotation"
renderQuickCreateSide QuickCreateEdit    = "o-add-modification"

data QuickCreateShowState =
    QuickCreateShown     -- ^ visible
  | QuickCreateNotShown  -- ^ will be visible when user selects a range
  | QuickCreateBlocked   -- ^ will not be shown even if user selects a range
  deriving (Show, Eq, Generic)


-- * Dialog

data CommentDisplayProps = CommentDisplayProps
  { _cdpCommentText  :: CommentText
  , _cdpIconStyle    :: IconDescription
  , _cdpUserName     :: JSString
  , _cdpCreationDate :: JSString
  , _cdpContentStyle :: [Style]
  , _cdpTopOffset    :: OffsetFromDocumentTop
  , _cdpWindowWidth  :: Int
  }
  deriving (Eq)

instance UnoverlapAllEq CommentDisplayProps

data ShowNoteProps =
    ShowNotePropsJust
      { _snpNote        :: Note
      , _snpTop         :: OffsetFromDocumentTop
      , _snpWindowWidth :: Int
      }
  | ShowNotePropsNothing
  deriving (Eq)

instance UnoverlapAllEq ShowNoteProps

data ShowDiscussionProps =
    ShowDiscussionPropsJust
      { _sdpNote        :: CompositeDiscussion
      , _sdpTop         :: OffsetFromDocumentTop
      , _sdpWindowWidth :: Int
      }
    | ShowDiscussionPropsNothing
  deriving (Eq)

instance UnoverlapAllEq ShowDiscussionProps

newtype ShowQuestionProps = ShowQuestionProps (Maybe CompositeQuestion)
  deriving (Eq)

instance UnoverlapAllEq ShowQuestionProps

data AddContributionProps kind = AddContributionProps
  { _acpVisible       :: Bool
  , _acpRange         :: Maybe Range
  , _acpKind          :: Maybe kind
  , _acpWindowWidth   :: Int
  }
  deriving (Eq)

instance UnoverlapAllEq (AddContributionProps CommentKind)
instance UnoverlapAllEq (AddContributionProps EditKind)


-- * boilerplate

makeRefineType ''MarkPosition
makeLenses ''MarkPositions

deriving instance NFData MarkPositions

instance ToJSON MarkPositions where
  toJSON = mapToValue . _markPositionsMap

instance FromJSON MarkPositions where
  parseJSON = fmap MarkPositions . mapFromValue

makeRefineType ''CommentInputState
makeRefineType ''ContributionAction
makeRefineType ''ContributionState
makeRefineType ''CommentKind
makeRefineType ''ActiveDialog

makeLenses ''BubbleProps
makeLenses ''SpecialBubbleProps
makeLenses ''QuickCreateProps

makeRefineType ''QuickCreateSide
makeRefineType ''QuickCreateShowState

makeLenses ''CommentDisplayProps
makeLenses ''AddContributionProps
