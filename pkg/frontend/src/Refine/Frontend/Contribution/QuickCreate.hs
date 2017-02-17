{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Refine.Frontend.Contribution.QuickCreate where

import           Control.Lens ((^.))
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux

import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Contribution.Types as RS
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.UtilityWidgets


quickCreate :: ReactView (String, (Maybe RS.Range, Maybe RS.DeviceOffset), SC.ScreenState)
quickCreate = defineView "QuickCreateButton" $ \(createType, currentSelection, screenState) ->
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        (Just range, Just deviceOffset) ->
            let offset = quickCreateOffset range deviceOffset screenState
            in positionedIconButton_
              (IconButtonProps
                (IconProps ("o-add-" <> createType) True ("icon-New_Comment", "bright") XXL)
                ""
                ""
                (fromString createType)
                ""
                False
                (\_ -> RS.dispatch (RS.ContributionAction RS.ClearSelection) <> RS.dispatch (RS.ContributionAction (RS.ShowCommentEditor (fst currentSelection))))
                []
              ) offset
        _ -> mempty
--    // quickCreate annotation ui events  -- RENAME: annotation => comment
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


quickCreateOffset :: RS.Range -> Int -> SC.ScreenState -> Int
quickCreateOffset range deviceOffset screenState =
    quickCreateSelectionTop range screenState +
    quickCreateSelectionPos range deviceOffset


-- | This is the offset from the bottom of the toolbar.
quickCreateSelectionTop :: RS.Range -> SC.ScreenState -> Int
quickCreateSelectionTop range = SC.offsetIntoText
  (SC.offsetFromDocumentTop (range ^. RS.rangeTopOffset) (range ^. RS.rangeScrollOffset))

quickCreateSelectionPos :: RS.Range -> Int -> Int
quickCreateSelectionPos range deviceOffset =
    let selectionHeight = (range ^. RS.rangeBottom) - (range ^. RS.rangeTopOffset . SC.unOffsetFromViewportTop)
        idealCenter = selectionHeight `div` 2 - 22
        useIdealCenter = selectionHeight <= 200
        closerToTop = abs (deviceOffset - range ^. RS.rangeTopOffset . SC.unOffsetFromViewportTop) < idealCenter
        edgePosition = if closerToTop then 0 else selectionHeight - 44
    in if useIdealCenter then idealCenter else edgePosition

-- "annotation" (RENAME: Comment), "modification" (RENAME: Edit)
quickCreate_ :: String -> (Maybe RS.Range, Maybe RS.DeviceOffset) -> SC.ScreenState -> ReactElementM eventHandler ()
quickCreate_ createType currentSelection screenState = view quickCreate (createType, currentSelection, screenState) mempty
