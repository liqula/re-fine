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
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import           Language.Css.Syntax hiding (Value)

import Refine.Common.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Frontend.Icon.Types
import Refine.Prelude.TH (makeRefineType)


newtype AddContributionFormState = AddContributionFormState
  { _addContributionFormState :: ST
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
    RequestSetRange -- ^ FIXME: move this to 'DocumentAction'
  | SetRange Range  -- ^ FIXME: move this to 'DocumentAction'
  | ClearRange      -- ^ FIXME: move this to 'DocumentAction'
  | ShowContributionDialog ContributionID
  | HideContributionDialog
  | ShowCommentEditor
  | HideCommentEditor
  | SetCommentKind CommentKind
  | SubmitComment ST (Maybe CommentKind)
  | RequestSetMarkPositions
  | SetMarkPositions [(ContributionID, MarkPosition)]  -- ^ see 'MarkPositions'
  | SetBubblePositioning BubblePositioning
  | HighlightMarkAndBubble [ContributionID]
  | SetBubbleFilter (Maybe (Set ContributionID))
  deriving (Show, Eq, Generic)


data ContributionState = ContributionState
  { _csCurrentRange             :: Maybe Range
  , _csCommentKind              :: Maybe CommentKind
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csActiveDialog             :: Maybe ActiveDialog
  , _csHighlightedMarkAndBubble :: [ContributionID]
  , _csQuickCreateShowState     :: QuickCreateShowState
  , _csMarkPositions            :: MarkPositions
  , _csBubblePositioning        :: BubblePositioning
  , _csBubbleFilter             :: Maybe (Set ContributionID)  -- ^ 'Nothing' means show everything.
  } deriving (Show, Eq, Generic)

data BubblePositioning = BubblePositioningAbsolute | BubblePositioningEvenlySpaced
  deriving (Show, Eq, Generic)

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
  , _csHighlightedMarkAndBubble = []
  , _csQuickCreateShowState     = QuickCreateNotShown
  , _csMarkPositions            = mempty
  , _csBubblePositioning        = BubblePositioningAbsolute
  , _csBubbleFilter             = Nothing
  }


-- * Bubble

data StackOrNot a = Stack (NonEmpty a) | NoStack a
  deriving (Eq, Ord, Show, Generic, Functor)

stackToHead :: StackOrNot a -> a
stackToHead (Stack (x :| _)) = x
stackToHead (NoStack x)      = x

stackToNonEmptyList :: StackOrNot a -> NonEmpty a
stackToNonEmptyList (Stack l)   = l
stackToNonEmptyList (NoStack x) = x :| []

stackToList :: StackOrNot a -> [a]
stackToList (Stack (x :| xs)) = x : xs
stackToList (NoStack x)       = [x]

data ProtoBubble = ProtoBubble
  { _protoBubbleContributionID :: ContributionID
  , _protoBubbleMarkPosition   :: MarkPosition
  , _protoBubbleChild          :: ReactElementM ViewEventHandler ()
  }

data BubbleProps = BubbleProps
  { _bubblePropsContributionIds   :: StackOrNot ContributionID
  , _bubblePropsIconSide          :: BubbleSide
  , _bubblePropsVerticalOffset    :: Maybe OffsetFromDocumentTop  -- ^ 'Nothing' means 'BubblePositioningEvenlySpaced'
  , _bubblePropsHighlight         :: Bool
  , _bubblePropsScreenState       :: ScreenState
  }
  deriving (Eq)

data BubbleSide = BubbleLeft | BubbleRight
  deriving (Eq)

instance Show BubbleSide where
  show BubbleLeft = "left"
  show BubbleRight = "right"

instance UnoverlapAllEq BubbleProps


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
  , _cdpContentStyle :: [Decl]
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

makeRefineType ''AddContributionFormState
makeRefineType ''ContributionAction
makeRefineType ''ContributionState
makeRefineType ''BubblePositioning
makeRefineType ''CommentKind
makeRefineType ''ActiveDialog

makeLenses ''ProtoBubble
makeLenses ''BubbleProps
makeLenses ''QuickCreateProps

makeRefineType ''QuickCreateSide
makeRefineType ''QuickCreateShowState

makeLenses ''CommentDisplayProps
makeLenses ''AddContributionProps
