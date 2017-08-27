{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Screen.Calculations where
#include "import_frontend.hs"

import           Refine.Frontend.Types
import           Refine.Frontend.Screen.Types


-- | This is the *actual* offset from page top (i think), and should perhaps be typed
-- 'OffsetFromHtmlTop'; and 'OffsetFromDocumentTop' then probably means 'OffsetFromArticleTop'.
-- None of this code is terribly easy to understand...  :(
offsetIntoText :: HasCallStack => OffsetFromDocumentTop -> ScreenState -> Int
offsetIntoText (OffsetFromDocumentTop topOffset) st = topOffset - st ^. ssHeaderHeight - 80

offsetFromDocumentTop :: HasCallStack => OffsetFromViewportTop -> ScrollOffsetOfViewport -> OffsetFromDocumentTop
offsetFromDocumentTop (OffsetFromViewportTop v) (ScrollOffsetOfViewport d) = OffsetFromDocumentTop (v + d)
