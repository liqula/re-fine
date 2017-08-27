{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.QuickCreate where
#include "import_frontend.hs"

import Refine.Frontend.Access
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon
import Refine.Frontend.Screen.Calculations
import Refine.Frontend.Screen.Types
import Refine.Frontend.Store()
import Refine.Frontend.Store.Types
import Refine.Frontend.Types


quickCreate :: HasCallStack => View '[QuickCreateProps]
quickCreate = mkView "QuickCreateButton" $ \props ->
    case (props ^. quickCreateRange, props ^. quickCreateShowState) of
        (Just range, QuickCreateShown) ->
            iconButton_ $ defaultIconButtonProps @QuickCreateSide
              & iconButtonPropsIconProps    .~ IconProps (renderQuickCreateSide (props ^. quickCreateSide))
                                                         True ("icon-New_Comment", "bright") XXLarge
              & iconButtonPropsPosition     .~ Just (mkQuickCreateOffset range (props ^. quickCreateScreenState))
              & iconButtonPropsOnClick      .~ (props ^. quickCreateSide)
        _ -> mempty
--    // quickCreate annotation ui events  -- RENAME: annotation => comment
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);

quickCreate_ :: HasCallStack => QuickCreateProps -> ReactElementM eventHandler ()
quickCreate_ = view_ quickCreate "quickCreate_"


instance IconButtonPropsOnClick QuickCreateSide where
  runIconButtonPropsOnClick _ _ = dispatch . \case
    QuickCreateComment -> LoginGuardStash [ContributionAction ShowCommentEditor]
    QuickCreateEdit    -> LoginGuardStash [HeaderAction StartEdit]
  defaultOnClick = QuickCreateComment


mkQuickCreateOffset :: HasCallStack => SelectionStateWithPx -> ScreenState -> Int
mkQuickCreateOffset range screenState =
    mkQuickCreateRangeTop range screenState +
    mkQuickCreateRangePos range

-- | This is the offset from the bottom of the toolbar.
mkQuickCreateRangeTop :: HasCallStack => SelectionStateWithPx -> ScreenState -> Int
mkQuickCreateRangeTop range = offsetIntoText
    (offsetFromDocumentTop (range ^. sstTopOffset) (range ^. sstScrollOffset))

mkQuickCreateRangePos :: HasCallStack => SelectionStateWithPx -> Int
mkQuickCreateRangePos range = if useIdealCenter then idealCenter else edgePosition
  where
    offsetFromTop   = range ^. sstDocTopOffset
    selectionHeight = (range ^. sstBottomOffset - range ^. sstTopOffset) ^. unOffsetFromViewportTop
    idealCenter     = selectionHeight `div` 2 - 22
    useIdealCenter  = selectionHeight <= 200
    closerToTop     = abs (offsetFromTop ^. unOffsetFromDocumentTop - range ^. sstTopOffset . unOffsetFromViewportTop) < idealCenter
    edgePosition    = if closerToTop then 0 else selectionHeight - 44
