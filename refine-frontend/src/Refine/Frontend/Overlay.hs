{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Overlay where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux

import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.UtilityWidgets


quickCreate :: ReactView (String, (Maybe RS.Range, Maybe RS.DeviceOffset), Int)
quickCreate = defineView "QuickCreateButton" $ \(createType, currentSelection, headerHeight) ->
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        (Just range, Just deviceOffset) ->
            let offset = quickCreateOffset range deviceOffset headerHeight
            in positionedIconButton_
              (IconButtonProps
                ("o-add-" <> createType)
                ""
                (fromString createType)
                True
                ("icon-New_Comment", "bright")
                ""
                XXL
                (\_ _ -> RS.dispatch RS.ShowCommentOverlay)
              ) offset
        _ -> div_ ""
--    // quickCreate annotation ui events
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


quickCreateOffset :: RS.Range -> Int -> Int -> Int
quickCreateOffset range deviceOffset headerHeight =
    quickCreateSelectionTop range headerHeight +
    quickCreateSelectionPos range deviceOffset


-- | This is the offset from the bottom of the toolbar.
quickCreateSelectionTop :: RS.Range -> Int -> Int
quickCreateSelectionTop range headerHeight = RS._top range + RS._scrollOffset range - headerHeight - 80

quickCreateSelectionPos :: RS.Range -> Int -> Int
quickCreateSelectionPos range deviceOffset =
    let selectionHeight = RS._bottom range - RS._top range
        idealCenter = selectionHeight `div` 2 - 22
        useIdealCenter = selectionHeight <= 200
        closerToTop = abs (deviceOffset - RS._top range) < idealCenter
        edgePosition = if closerToTop then 0 else selectionHeight - 44
    in if useIdealCenter then idealCenter else edgePosition

-- "annotation", "modification"
quickCreate_ :: String -> (Maybe RS.Range, Maybe RS.DeviceOffset) -> Int -> ReactElementM eventHandler ()
quickCreate_ createType currentSelection headerHeight = view quickCreate (createType, currentSelection, headerHeight) mempty
