{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.QuickCreate where
#include "import_frontend.hs"

import Refine.Frontend.Access
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon
import Refine.Frontend.Icon.Svg as Svg
import Refine.Frontend.Screen.Calculations
import Refine.Frontend.Screen.Types
import Refine.Frontend.Store()
import Refine.Frontend.Store.Types
import Refine.Frontend.Types


quickCreate :: HasCallStack => View '[QuickCreateProps]
quickCreate = mkView "QuickCreateButton" $ \props -> do
    case (props ^. quickCreateRange, props ^. quickCreateShowState) of
        (Just range, QuickCreateShown) -> do
          let pos = mkQuickCreateOffset range (props ^. quickCreateScreenState)
          div_ ["style" @= object ["top" Aeson..= pos]] $ do
            let img = case props ^. quickCreateSide of
                  QuickCreateComment -> Svg.CommentNew
                  QuickCreateEdit    -> Svg.EditNew
            ibutton_ $ emptyIbuttonProps
              (ButtonImageIcon img ColorSchemaDark)
              (props ^. quickCreateSide)
              & ibSize .~ XXXLarge
        _ -> pure ()

instance IbuttonOnClick QuickCreateSide 'EventHandlerCode where
  runIbuttonOnClick _ _ = dispatch . \case
    QuickCreateComment -> LoginGuardStash [ContributionAction ShowCommentEditor]
    QuickCreateEdit    -> LoginGuardStash [HeaderAction StartEdit]

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
