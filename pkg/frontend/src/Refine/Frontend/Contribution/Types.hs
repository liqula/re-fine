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
import           Data.Aeson (toJSON, parseJSON, object, (.=), withObject)
import           Data.Aeson.Types (FromJSON, ToJSON, Value, Parser)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import           Data.String.Conversions
import           GHC.Generics (Generic)
import           GHCJS.Types
import           Text.HTML.Parser (Attr)
import           Text.Read (readMaybe)
import           React.Flux (UnoverlapAllEq)

import Refine.Common.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Prelude.TH (makeRefineType)


newtype CommentInputState = CommentInputState
  { _commentInputStateText :: ST
  } deriving (Show, Eq, Generic)


-- | Mark positions updates have experienced long cascades of changing values for the same
-- 'ContributionID'.  Now the 'ScheduleAddMarkPosition' handler will store new additions in a
-- separate `scheduled` map, and a 'DischargeAddMarkPositions' will be dispatched with a delay.  If
-- the 'DischargeAddMarkPositions' handler finds something in the `scheduled` map, it will add those
-- and remove them there; otherwise it will do nothing.
--
-- TODO: We should find out where the cascades actually come from and find a better fix: (1) the
-- instances of 'Eq', 'FromJSON', 'ToJSON' do not consider the '_markPositionsScheduled' field,
-- which is necessary for the 'DischargeAddMarkPositions' hack to work, but confusing; (2) we're
-- still receiving too many 'ScheduleAddMarkPosition' actions, and should rather figure out how to
-- only fire those that are necessary.
newtype MarkPositions = MarkPositions { _markPositionsMap :: M.Map ContributionID MarkPosition }
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
mapToValue :: (Show k, ToJSON v) => M.Map k v -> Value
mapToValue = object . fmap (\(k, v) -> (cs . show) k .= v) . M.toList

mapFromValue :: (Ord k, Read k, FromJSON v) => Value -> Parser (M.Map k v)
mapFromValue = withObject "MarkPositions"
  $ fmap M.fromList
  . mapM (\(k, v) -> (,) <$> maybe (fail "could not parse key.") pure (readMaybe (cs k))
                         <*> parseJSON v)
  . HashMap.toList


-- | TODO: give record selectors to all fields.
data ContributionAction =
    SetRange Range  -- TODO: move this to 'DocumentAction'
  | ClearRange      -- TODO: move this to 'DocumentAction'
  | ShowContributionDialog ContributionID
  | HideCommentOverlay
  | ShowCommentEditor
  | HideCommentEditor
  | SetCommentKind CommentKind
  | SubmitComment ST (Maybe CommentKind)
  | AddMarkPosition ContributionID MarkPosition  -- ^ see 'MarkPosition'
  | HighlightMarkAndBubble ContributionID
  | UnhighlightMarkAndBubble
  deriving (Show, Eq, Generic)


data ContributionState = ContributionState
  { _csCurrentRange             :: Maybe Range
  , _csCommentKind              :: Maybe CommentKind
  , _csDisplayedContributionID  :: Maybe ContributionID
  , _csCommentEditorVisible     :: Bool  -- ^ (the comment or edit dialog, that is.  not the vdoc editor.)
  , _csHighlightedMarkAndBubble :: Maybe ContributionID
  , _csQuickCreateShowState     :: QuickCreateShowState
  , _csMarkPositions            :: MarkPositions
  } deriving (Show, Eq, Generic)

data CommentKind =
    CommentKindNote
  -- | CommentKindQuestion
  | CommentKindDiscussion
  deriving (Show, Eq, Generic)

emptyContributionState :: ContributionState
emptyContributionState = ContributionState
  { _csCurrentRange             = Nothing
  , _csCommentKind              = Nothing
  , _csDisplayedContributionID  = Nothing
  , _csCommentEditorVisible     = False
  , _csHighlightedMarkAndBubble = Nothing
  , _csQuickCreateShowState     = QuickCreateNotShown
  , _csMarkPositions            = mempty
  }


data MarkProps = MarkProps
  { _markPropsAttrs                 :: [Attr]
  , _markPropsContributionID        :: ContributionID
  , _markPropsHighlightedMark       :: Maybe ContributionID
  , _markPropsDisplayedContribution :: Maybe ContributionID
  }
  deriving (Eq)

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

makeLenses ''MarkProps
makeLenses ''BubbleProps
makeLenses ''SpecialBubbleProps
makeLenses ''QuickCreateProps

makeRefineType ''QuickCreateSide
makeRefineType ''QuickCreateShowState
