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
{-# LANGUAGE NoImplicitPrelude          #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.Types where

import Refine.Frontend.Prelude

import           Control.DeepSeq
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import           Language.Css.Syntax hiding (Value)

import Refine.Common.Types
import Refine.Frontend.Icon.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Frontend.Util
import Refine.Prelude.TH (makeRefineType)


newtype AllVertialSpanBounds = AllVertialSpanBounds { _allVertialSpanBounds :: Map.Map ContributionID VertialSpanBounds }
  deriving (Show, Eq, Generic, Monoid)

data VertialSpanBounds = VertialSpanBounds
  { _vertialSpanBoundsTop    :: OffsetFromDocumentTop
  , _vertialSpanBoundsBottom :: OffsetFromDocumentTop
  }
  deriving (Eq, Show, Generic)

-- | TODO: we have orphan instances for maps in Refine.Common.Orphans.  we should:
-- (1) move this function there;
-- (2) implement the orphan instances in terms of this function, not via lists;
-- (3) same for @mapFromValue@.
-- (4) rename to @map{From,To}JSON@.
mapToValue :: HasCallStack => (Show k, ToJSON v) => Map.Map k v -> Value
mapToValue = object . fmap (\(k, v) -> (cs . show) k .:= v) . Map.toList

mapFromValue :: HasCallStack => (Ord k, Read k, FromJSON v) => Value -> Parser (Map.Map k v)
mapFromValue = withObject "AllVertialSpanBounds"
  $ fmap Map.fromList
  . mapM (\(k, v) -> (,) <$> maybe (fail "could not parse key.") pure (readMaybe (cs k))
                         <*> parseJSON v)
  . HashMap.toList


-- * Contribution

data ContributionAction =
    RequestSetRange
  | SetRange SelectionStateWithPx
  | ClearRange
  | ShowContributionDialog ContributionID
  | HideContributionDialog
  | ShowCommentEditor
  | HideCommentEditor
  | SubmitComment (CommentInfo CommentKind)
  | RequestSetAllVertialSpanBounds
  | SetAllVertialSpanBounds [(ContributionID, VertialSpanBounds)]  -- ^ see 'AllVertialSpanBounds'
  | SetBubblePositioning BubblePositioning
  | HighlightMarkAndBubble [ContributionID]
  | SetBubbleFilter (Maybe (Set ContributionID))
  | ToggleVoteOnContribution (ID Edit) Vote
  deriving (Show, Eq, Generic)


data ContributionState = ContributionState
  { _csCurrentSelectionWithPx   :: Maybe SelectionStateWithPx
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csActiveDialog             :: Maybe ActiveDialog
  , _csHighlightedMarkAndBubble :: [ContributionID]
  , _csQuickCreateShowState     :: QuickCreateShowState
  , _csAllVertialSpanBounds     :: AllVertialSpanBounds
  , _csBubblePositioning        :: BubblePositioning
  , _csBubbleFilter             :: Maybe (Set ContributionID)  -- ^ 'Nothing' means show everything.
  } deriving (Show, Eq, Generic)

data BubblePositioning = BubblePositioningAbsolute | BubblePositioningEvenlySpaced
  deriving (Show, Eq, Generic)

-- | The state of 'commentInput_' consists of the 'CommentInfo'
-- entered by the user, plus two mouse-over booleans for the
-- 'CommentKind' toggle buttons.
data CommentInputState = CommentInputState
  { _commentInputStateData                :: CommentInfo (Maybe CommentKind)
  , _commentInputStateMouseOverNote       :: Bool
  , _commentInputStateMouseOverDiscussion :: Bool
  }
  deriving (Show, Eq, Generic)

data CommentInfo kind = CommentInfo { _commentInfoDesc :: ST, _commentInfoKind :: kind }
  deriving (Show, Eq, Generic)

data CommentKind =
    CommentKindNote
  -- | CommentKindQuestion
  | CommentKindDiscussion
  deriving (Show, Eq, Generic)

-- | Like 'CommentInputState', but for 'editInput_'.
data EditInputState = EditInputState
  { _editInputStateData      :: EditInfo (Maybe EditKind)
  , _editInputStateMouseOver :: Maybe EditKind
  }
  deriving (Show, Eq, Generic)

data EditInfo kind = EditInfo { _editInfoDesc :: ST, _editInfoKind :: kind }
  deriving (Show, Eq, Generic)

data ActiveDialog = ActiveDialogComment | ActiveDialogEdit
  deriving (Show, Eq, Generic)

emptyContributionState :: HasCallStack => ContributionState
emptyContributionState = ContributionState
  { _csCurrentSelectionWithPx   = Nothing
  , _csDisplayedContributionID  = Nothing
  , _csActiveDialog             = Nothing
  , _csHighlightedMarkAndBubble = []
  , _csQuickCreateShowState     = QuickCreateNotShown
  , _csAllVertialSpanBounds     = mempty
  , _csBubblePositioning        = BubblePositioningAbsolute
  , _csBubbleFilter             = Nothing
  }


-- * Bubble

data StackOrNot a = Stack (NonEmpty a) | NoStack a
  deriving (Eq, Ord, Show, Generic, Functor)

stackToHead :: HasCallStack => StackOrNot a -> a
stackToHead (Stack (x :| _)) = x
stackToHead (NoStack x)      = x

stackToNonEmptyList :: HasCallStack => StackOrNot a -> NonEmpty a
stackToNonEmptyList (Stack l)   = l
stackToNonEmptyList (NoStack x) = x :| []

stackToList :: HasCallStack => StackOrNot a -> [a]
stackToList (Stack (x :| xs)) = x : xs
stackToList (NoStack x)       = [x]

data ProtoBubble = ProtoBubble
  { _protoBubbleContributionID    :: ContributionID
  , _protoBubbleVertialSpanBounds :: VertialSpanBounds
  , _protoBubbleChild             :: ReactElementM ViewEventHandler ()
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
  , _quickCreateRange       :: Maybe SelectionStateWithPx
  , _quickCreateScreenState :: ScreenState
  }
  deriving (Show, Eq)

data QuickCreateSide = QuickCreateComment | QuickCreateEdit
  deriving (Show, Eq, Generic)

renderQuickCreateSide :: HasCallStack => QuickCreateSide -> JSString
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

data AddContributionProps st = AddContributionProps
  { _acpVisible       :: Bool
  , _acpRange         :: Maybe SelectionStateWithPx
  , _acpLocalState    :: st
  , _acpWindowWidth   :: Int
  }
  deriving (Eq)

instance UnoverlapAllEq (AddContributionProps ())
instance UnoverlapAllEq (AddContributionProps (Maybe EditKind))
instance UnoverlapAllEq (AddContributionProps (EditInfo (Maybe EditKind)))


-- * instances

makeRefineType ''VertialSpanBounds
makeLenses ''AllVertialSpanBounds

deriving instance NFData AllVertialSpanBounds

instance ToJSON AllVertialSpanBounds where
  toJSON = mapToValue . _allVertialSpanBounds

instance FromJSON AllVertialSpanBounds where
  parseJSON = fmap AllVertialSpanBounds . mapFromValue

makeRefineType ''ContributionAction
makeRefineType ''ContributionState
makeRefineType ''BubblePositioning

makeRefineType ''CommentInputState
makeLenses ''CommentInfo
makeRefineType' [t| CommentInfo CommentKind |]
makeRefineType' [t| CommentInfo (Maybe CommentKind) |]

makeRefineType ''EditInputState
makeLenses ''EditInfo
makeRefineType' [t| EditInfo EditKind |]
makeRefineType' [t| EditInfo (Maybe EditKind) |]

makeRefineType ''CommentKind
makeRefineType ''ActiveDialog

makeLenses ''ProtoBubble
makeLenses ''BubbleProps
makeLenses ''QuickCreateProps

makeRefineType ''QuickCreateSide
makeRefineType ''QuickCreateShowState

makeLenses ''CommentDisplayProps
makeLenses ''AddContributionProps

instance IbuttonOnClick CommentKind (StatefulViewEventHandler CommentInputState) where
  runIbuttonOnClick _evt _mevt ckind st = (mempty, Just $ st & commentInputStateData . commentInfoKind .~ Just ckind)

instance IbuttonOnClick EditKind (StatefulViewEventHandler EditInputState) where
  runIbuttonOnClick _evt _mevt ekind st = (mempty, Just $ st & editInputStateData . editInfoKind .~ Just ekind)


-- * helpers

scrollToCurrentSelection :: HasCallStack => MonadIO m => ContributionState -> m ()
scrollToCurrentSelection = liftIO . js_scrollToPx . currentSelectionOffset

currentSelectionOffset :: HasCallStack => ContributionState -> Int
currentSelectionOffset st = fromMaybe 0 $ do
  sel <- st ^? csCurrentSelectionWithPx . _Just
  pure $ (sel ^. sstBottomOffset . unOffsetFromViewportTop)
       + (sel ^. sstScrollOffset . unScrollOffsetOfViewport)
       + tweakScrollTarget

-- | FUTUREWORK: come up with a more robust way to move the dialog box into the center of the view.
tweakScrollTarget :: HasCallStack => Int
tweakScrollTarget = -170
