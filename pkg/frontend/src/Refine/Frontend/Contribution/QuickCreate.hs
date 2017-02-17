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

import qualified Refine.Frontend.Contribution.Types as RS
import qualified Refine.Frontend.Header.Types as RS
import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets


quickCreate :: ReactView QuickCreateProps
quickCreate = defineView "QuickCreateButton" $ \(QuickCreateProps createType currentSelection screenState displayInfo) ->
  if displayInfo == RS.CommentToolbarExtensionWithSelection then mempty -- do not display the buttons when selection was activated via toolbar
  else
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        RS.RangeSelected range offsetFromTop ->
            let offset = quickCreateOffset range offsetFromTop screenState
            in positionedIconButton_
              (IconButtonProps
                (IconProps ("o-add-" <> createType) True ("icon-New_Comment", "bright") XXL)
                ""
                ""
                (fromString createType)
                ""
                False
                (\_ -> RS.dispatch . RS.ContributionAction . RS.ShowCommentEditor $ Just range)
                []
              ) offset
        _ -> mempty
--    // quickCreate annotation ui events  -- RENAME: annotation => comment
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


quickCreateOffset :: RS.Range -> SC.OffsetFromDocumentTop -> SC.ScreenState -> Int
quickCreateOffset range offsetFromTop screenState =
    quickCreateSelectionTop range screenState +
    quickCreateSelectionPos range offsetFromTop


-- | This is the offset from the bottom of the toolbar.
quickCreateSelectionTop :: RS.Range -> SC.ScreenState -> Int
quickCreateSelectionTop range = SC.offsetIntoText
  (SC.offsetFromDocumentTop (range ^. RS.rangeTopOffset) (range ^. RS.rangeScrollOffset))
      -- FIXME: should Range contain an OffsetFromDocumentTop instead?

quickCreateSelectionPos :: RS.Range -> SC.OffsetFromDocumentTop -> Int
quickCreateSelectionPos range offsetFromTop =
    let selectionHeight = (range ^. RS.rangeBottom) - (range ^. RS.rangeTopOffset . SC.unOffsetFromViewportTop)
        idealCenter = selectionHeight `div` 2 - 22
        useIdealCenter = selectionHeight <= 200
        closerToTop = abs (offsetFromTop ^. SC.unOffsetFromDocumentTop - range ^. RS.rangeTopOffset . SC.unOffsetFromViewportTop) < idealCenter
        edgePosition = if closerToTop then 0 else selectionHeight - 44
    in if useIdealCenter then idealCenter else edgePosition

data QuickCreateProps = QuickCreateProps
  { _quickCreateContributionkind :: String -- TODO use the proper data type here
  , _quickCreateRange :: RS.Selection
  , _quickCreateOffset :: SC.ScreenState
  , _quickCreateInfo :: RS.ToolbarExtensionStatus
  }

-- "annotation" (RENAME: Comment), "modification" (RENAME: Edit)
quickCreate_ :: QuickCreateProps -> ReactElementM eventHandler ()
quickCreate_ props = view quickCreate props mempty
