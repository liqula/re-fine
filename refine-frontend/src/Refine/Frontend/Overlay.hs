{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Overlay where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux

import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets


quickCreate :: ReactView (String, (Maybe RS.Range, Maybe RS.DeviceOffset), Int)
quickCreate = defineView "QuickCreateButton" $ \(createType, currentSelection, headerHeight) ->
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        (Just range, Just deviceOffset) ->
            let offset = quickCreateOffset range deviceOffset headerHeight
            in positionedIconButton_ (IconButtonProps ("o-add-" <> createType) "" (fromString createType) True ("icon-New_Comment", "bright") "" XXL) offset
        _ -> div_ ""
--    // quickCreate annotation ui events
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


quickCreateOffset :: RS.Range -> Int -> Int -> Int
quickCreateOffset range _deviceOffset headerHeight =
    let topOfSelection = RS._top range + RS._scrollOffset range - headerHeight - 80 -- offset from menu bar
        idealCenter = (RS._bottom range - RS._top range) `div` 2 - 22
        usefulCenter = idealCenter -- if abs (deviceOffset - idealCenter) > 200 then deviceOffset else idealCenter
    in topOfSelection + usefulCenter


-- "annotation", "modification"
quickCreate_ :: String -> (Maybe RS.Range, Maybe RS.DeviceOffset) -> Int -> ReactElementM eventHandler ()
quickCreate_ createType currentSelection headerHeight = view quickCreate (createType, currentSelection, headerHeight) mempty
