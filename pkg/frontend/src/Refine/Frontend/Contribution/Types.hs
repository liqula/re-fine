{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.Types where
#include "import_frontend.hs"

import Control.DeepSeq
import Data.List (nubBy)
import Language.Css.Syntax hiding (Value)

import React.Flux.Missing
import Refine.Common.Types
import Refine.Common.VDoc.OT
import Refine.Frontend.Document.FFI.Types (EditorState)
import Refine.Frontend.Icon.Svg
import Refine.Frontend.Icon.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Frontend.Util


-- | Keep track of *all* 'MarkID' values, which includes 'ContributionID's and the current selection
-- ("highlighted mark").  (Is the latter used in "Contribution.QuickCreate"?  Not sure right now..)
newtype AllVerticalSpanBounds = AllVerticalSpanBounds { _allVerticalSpanBounds :: Map.Map MarkID VerticalSpanBounds }
  deriving (Show, Eq, Generic, Monoid)

data VerticalSpanBounds = VerticalSpanBounds
  { _verticalSpanBoundsTop    :: OffsetFromDocumentTop
  , _verticalSpanBoundsBottom :: OffsetFromDocumentTop
  }
  deriving (Eq, Show, Generic)

-- | FIXME: we have orphan instances for maps in Refine.Common.Orphans.  we should:
-- (1) move this function there;
-- (2) implement the orphan instances in terms of this function, not via lists;
-- (3) same for @mapFromValue@.
-- (4) rename to @map{From,To}JSON@.
mapToValue :: HasCallStack => (Show k, ToJSON v) => Map.Map k v -> Value
mapToValue = object . fmap (\(k, v) -> (cs . show) k .:= v) . Map.toList

mapFromValue :: HasCallStack => (Ord k, Read k, FromJSON v) => Value -> Parser (Map.Map k v)
mapFromValue = withObject "AllVerticalSpanBounds"
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
  | RequestSetAllVerticalSpanBounds
  | SetAllVerticalSpanBounds [(MarkID, VerticalSpanBounds)]  -- ^ see 'AllVerticalSpanBounds'
  | SetBubblePositioning BubblePositioning
  | HighlightMarkAndBubble [MarkID]
  | SetBubbleFilter (Maybe (Set ContributionID))
  | ToggleVoteOnContribution ContributionID Vote
  deriving (Show, Eq, Generic)

data ContributionState = ContributionState
  { _csCurrentSelectionWithPx   :: Maybe SelectionStateWithPx
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csActiveDialog             :: Maybe ActiveDialog
  , _csHighlightedMarkAndBubble :: [MarkID]
  , _csQuickCreateShowState     :: QuickCreateShowState
  , _csAllVerticalSpanBounds    :: AllVerticalSpanBounds
  , _csBubblePositioning        :: BubblePositioning
  , _csBubbleFilter             :: Maybe (Set ContributionID)  -- ^ 'Nothing' means show everything.
  } deriving (Show, Eq, Generic)

data BubblePositioning = BubblePositioningAbsolute | BubblePositioningEvenlySpaced
  deriving (Show, Eq, Generic)

emptyContributionState :: HasCallStack => ContributionState
emptyContributionState = ContributionState
  { _csCurrentSelectionWithPx   = Nothing
  , _csDisplayedContributionID  = Nothing
  , _csActiveDialog             = Nothing
  , _csHighlightedMarkAndBubble = []
  , _csQuickCreateShowState     = QuickCreateNotShown
  , _csAllVerticalSpanBounds    = mempty
  , _csBubblePositioning        = BubblePositioningAbsolute
  , _csBubbleFilter             = Nothing
  }


-- | The state of 'commentInput_' consists of the 'CommentInfo'
-- entered by the user, plus two mouse-over booleans for the
-- 'CommentKind' toggle buttons.
data CommentInputState = CommentInputState
  { _commentInputStateData                :: CommentInfo (Maybe CommentKind)
  , _commentInputStateNoteButton          :: ButtonState
  , _commentInputStateDiscussionButton    :: ButtonState
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
  { _editInputStateData    :: EditInfo (Maybe EditKind)
  , _editInputStateButtons :: [(EditKind, ButtonState)]
  }
  deriving (Show, Eq, Generic)

-- | FIXME: it's weird that there is are two 'editInfoDesc' values in here: one at @^. editInfoDesc@
-- and one at @^. editInfoLocalStateRef . <deref> . editInputStateData . editInfoDesc'.  (In fact,
-- it's an infinite tree?!  the knot is tied on creating the ref, but still.)  there may be a way to
-- implement this that's easier to understand.
data EditInfo kind = EditInfo
  { _editInfoDesc :: ST
  , _editInfoKind :: kind
  , _editInfoLocalStateRef :: LocalStateRef EditInputState
  }
  deriving (Show, Eq, Generic)

cleanupEditInputStateButtons :: [(EditKind, ButtonState)] -> [(EditKind, ButtonState)]
cleanupEditInputStateButtons = f . g . nubBy ((==) `on` fst)
  where
    f (x@(_, ButtonState Pressed _) : xs) = x : f ((_2 . buttonPressed .~ Released) <$> xs)
    f (x : xs) = x : f xs
    f [] = []

    g (x@(_, ButtonState _ RollOver) : xs) = x : g ((_2 . buttonRollOver .~ NotRollOver) <$> xs)
    g (x : xs) = x : g xs
    g [] = []

data ActiveDialog = ActiveDialogComment (LocalStateRef CommentInputState) | ActiveDialogEdit EditorState
  deriving (Show, Eq, Generic)


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
  { _protoBubbleContributionID     :: (ContributionID, Int)
  , _protoBubbleVerticalSpanBounds :: VerticalSpanBounds
  , _protoBubbleChild              :: ReactElementM 'EventHandlerCode ()
  }

data BubbleProps = BubbleProps
  { _bubblePropsContributionIds   :: StackOrNot (ContributionID, Int)
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
  , _cdpNoteId       :: ID Discussion
  , _cdpVotes        :: VoteCount
  }
  deriving (Eq)

data ShowNoteProps =
    ShowNoteProps
      { _snpNote        :: Discussion
      , _snpTop         :: OffsetFromDocumentTop
      , _snpWindowWidth :: Int
      , _snpUsernames   :: Map (ID User) Username -- FIXME: store user names in discussions
      }
  deriving (Eq)

data AddContributionProps st = AddContributionProps
  { _acpRange         :: Maybe SelectionStateWithPx
  , _acpLocalState    :: st
  , _acpWindowWidth   :: Int
  }
  deriving (Eq)


data DiscussionProps = DiscussionProps
  { _discPropsDiscussion :: Either (ID Discussion) (Range Position, Discussion)
  , _discPropsAboutText  :: RawContent  -- ^ the blocks overlapping the range of the discussion.
  , _discPropsDetails    :: StatementPropDetails
  , _discPropsFlatView   :: Bool
  }
  deriving (Eq, Show, Generic)

data StatementPropDetails = StatementPropDetails
  { _spdEditorProps :: Maybe StatementEditorProps
  , _spdCurrentUser :: Maybe (ID User)
  , _spdUsernames   :: Map (ID User) Username -- FIXME: store user names in discussions
  }
  deriving (Eq, Show, Generic)

data StatementEditorProps = StatementEditorProps
  { _sepStatementID :: ID Statement
  , _sepLocalState  :: LocalStateRef CreateStatement
  , _sepUpdate      :: Bool
  }
  deriving (Eq, Show, Generic)

-- data DiscussionMode = DiscussionModeChrono | DiscussionModeTree
--   deriving (Eq, Ord, Show, Bounded, Enum, Generic)

discussionProps :: Either (ID Discussion) (Range Position, Discussion)  -> RawContent -> StatementPropDetails -> Bool -> DiscussionProps
discussionProps d@(Right disc) = DiscussionProps d . cropToBlocks (fst disc)
discussionProps d = DiscussionProps d . const (mkRawContent $ mkBlock hourglass :| [])

-- | Remove all blocks that do not overlap with a range.
--
-- FIXME: where should we move this?  (also move the test cases if you move this!)
cropToBlocks :: Range Position -> RawContent -> RawContent
cropToBlocks rnge = toRawContent . prune . fromRawContent
  where
    prune :: NewDoc -> NewDoc
    prune d1 = case splt endIx d1 of
                 (d2, _) -> case splt startIx d2 of
                   (_, d3) -> d3

    startIx = rnge ^. rangeBegin . rowIndex
    endIx   = rnge ^. rangeEnd . rowIndex + 1

    splt :: Int -> NewDoc -> (NewDoc, NewDoc)
    splt = split (mempty, measureRowColumn . _1)

-- | FIXME: where should we move this?  (also move the test cases if you move this!)
blockIndices :: RawContent -> [BlockIndex]
blockIndices (RawContent blocks _) = zipWith BlockIndex [0..] . fmap (view blockKey) . NEL.toList $ blocks


-- * instances

deriveClasses
  [ ([''VerticalSpanBounds, ''ContributionAction, ''ContributionState, ''BubblePositioning, ''CommentInputState, ''EditInputState, ''CommentKind, ''ActiveDialog, ''QuickCreateSide, ''QuickCreateShowState, ''StatementPropDetails, ''StatementEditorProps], allClass)
  , ([''AllVerticalSpanBounds, ''CommentInfo, ''EditInfo, ''ProtoBubble, ''BubbleProps, ''QuickCreateProps, ''CommentDisplayProps, ''AddContributionProps], [''Lens'])
  ]

makeRefineType' [t| CommentInfo CommentKind |]
makeRefineType' [t| CommentInfo (Maybe CommentKind) |]
makeRefineType' [t| EditInfo EditKind |]
makeRefineType' [t| EditInfo (Maybe EditKind) |]

deriving instance NFData AllVerticalSpanBounds

instance ToJSON AllVerticalSpanBounds where
  toJSON = mapToValue . _allVerticalSpanBounds

instance FromJSON AllVerticalSpanBounds where
  parseJSON = fmap AllVerticalSpanBounds . mapFromValue

instance IbuttonOnClick CommentKind ('StatefulEventHandlerCode CommentInputState) where
  runIbuttonOnClick _evt _mevt ckind st = (mempty, Just $ st & commentInputStateData . commentInfoKind .~ Just ckind)

instance IbuttonOnClick EditKind ('StatefulEventHandlerCode EditInputState) where
  runIbuttonOnClick _evt _mevt ekind st = (mempty, Just $ st & editInputStateData . editInfoKind .~ Just ekind)

deriveClasses [([''DiscussionProps], allClass)]


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
