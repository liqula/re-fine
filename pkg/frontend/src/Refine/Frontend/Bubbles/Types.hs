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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Bubbles.Types where

import           Control.DeepSeq
import           Control.Lens (makeLenses)
import           Data.Aeson (toJSON, parseJSON, object, (.=), (.:), (.:?), withObject)
import           Data.Aeson.Types (FromJSON, ToJSON, Value, Parser)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import           Data.String.Conversions
import           Data.Void
import           GHC.Generics (Generic)
import           Text.Read (readMaybe)

import Refine.Common.Types
import Refine.Prelude.TH (makeRefineType)


data Range = Range  -- FIXME: selectors should all have the prefix _range for disambiguation.
    { _startPoint   :: Maybe ChunkPoint
    , _endPoint     :: Maybe ChunkPoint
    , _top          :: Int
    , _bottom       :: Int
    , _scrollOffset :: Int
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

type DeviceOffset = Int

type Selection = (Maybe Range, Maybe DeviceOffset)

-- for Overlay:
newtype CommentInputState = CommentInputState
  { _commentInputStateText     :: ST
  } deriving (Show, Generic)

data CommentCategory =
    Discussion
  | Note
  deriving (Show, Generic)

-- for marks:
newtype MarkPositions = MarkPositions { _unMarkPositions :: M.Map (ID Void) (Int, Int) }
  deriving (Eq, Show, Generic, NFData)

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

instance ToJSON MarkPositions where
  toJSON = mapToValue . _unMarkPositions

instance FromJSON MarkPositions where
  parseJSON = fmap MarkPositions . mapFromValue


data BubblesAction =
    UpdateSelection Selection
  | ClearSelection
  | ShowNoteOverlay (ID Note)
  | ShowDiscussionOverlay (ID Discussion)
  | HideCommentOverlay
  | ShowCommentEditor (Maybe Range)
  | HideCommentEditor
  | SetCommentCategory CommentCategory
  | SubmitComment ST (Maybe CommentCategory) (Maybe Range)
  | SubmitEdit
  deriving (Show, Generic)


data BubblesState = BubblesState
  { _bsCurrentSelection       :: Selection
  , _bsCommentCategory        :: Maybe CommentCategory
  , _bsDiscussionIsVisible    :: Maybe (ID Discussion)
  , _bsNoteIsVisible          :: Maybe (ID Note)
  , _bsCommentEditorIsVisible :: (Bool, Maybe Range)
  , _bsMarkPositions          :: MarkPositions
  } deriving (Show, Generic)


emptyBubblesState :: BubblesState
emptyBubblesState = BubblesState (Nothing, Nothing) Nothing Nothing Nothing (False, Nothing) (MarkPositions M.empty)


makeRefineType ''CommentInputState
makeRefineType ''CommentCategory
makeRefineType ''BubblesAction
makeRefineType ''BubblesState
