{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Views.Types where
#include "import_frontend.hs"

import           Refine.Common.Types (MarkID, Discussion, Edit)
import           Refine.Frontend.Contribution.Types (AllVerticalSpanBounds, BubblePositioning, QuickCreateShowState)
import           Refine.Frontend.Screen.Types (ScreenState)
import           Refine.Frontend.Types


data AsideProps contrib = AsideProps
  { _asideAllVerticalSpanBounds :: AllVerticalSpanBounds
  , _asideMinimumSpanYPos       :: OffsetFromDocumentTop
  , _asideCurrentRange          :: Maybe SelectionStateWithPx
  , _asideHighlighteds          :: [MarkID]
  , _asideScreenState           :: ScreenState
  , _asideContributions         :: [contrib]
  , _asideBubblePositioning     :: BubblePositioning
  , _asideQuickCreateShow       :: QuickCreateShowState
  }
  deriving (Eq)

makeLenses ''AsideProps
