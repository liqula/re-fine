{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Views.Types where

import Refine.Frontend.Prelude

import           Control.Lens (makeLenses)

import           Refine.Common.Types (MarkID, Discussion, Note, Edit)
import           Refine.Frontend.Contribution.Types (AllVerticalSpanBounds, BubblePositioning, QuickCreateShowState)
import           Refine.Frontend.Screen.Types (ScreenState)
import           Refine.Frontend.Types


data AsideProps = AsideProps
  { _asideAllVerticalSpanBounds :: AllVerticalSpanBounds
  , _asideMinimumSpanYPos       :: OffsetFromDocumentTop
  , _asideCurrentRange          :: Maybe SelectionStateWithPx
  , _asideHighlighteds          :: [MarkID]
  , _asideScreenState           :: ScreenState
  , _asideDiscussions           :: [Discussion]
  , _asideNotes                 :: [Note]
  , _asideEdits                 :: [Edit]
  , _asideBubblePositioning     :: BubblePositioning
  , _asideQuickCreateShow       :: QuickCreateShowState
  }
  deriving (Eq)

makeLenses ''AsideProps
