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

import           Control.Lens (makeLenses)
import qualified Data.Aeson as AE
import           Control.DeepSeq
import           GHC.Generics (Generic)
import           Data.String.Conversions

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

instance AE.FromJSON Range where
    parseJSON = AE.withObject "Range" $ \v -> Range <$>
                             v AE..:? "start" <*>
                             v AE..:? "end" <*>
                             v AE..: "top" <*>
                             v AE..: "bottom" <*>
                             v AE..: "scrollOffset"

instance AE.ToJSON Range where
    toJSON (Range sp ep t b s) = AE.object
      [ "start"        AE..= sp
      , "end"          AE..= ep
      , "top"          AE..= t
      , "bottom"       AE..= b
      , "scrollOffset" AE..= s
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
  , _bsCommentIsVisible       :: Bool
  , _bsCommentEditorIsVisible :: (Bool, Maybe Range)
  } deriving (Show, Generic)

emptyBubblesState :: BubblesState
emptyBubblesState = BubblesState (Nothing, Nothing) Nothing False (False, Nothing)


makeRefineType ''CommentInputState
makeRefineType ''CommentCategory
makeRefineType ''BubblesAction
makeRefineType ''BubblesState
