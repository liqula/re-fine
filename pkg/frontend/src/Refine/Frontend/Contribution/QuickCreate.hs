{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.QuickCreate where

import Refine.Frontend.Prelude

import           Control.Lens ((^.), (&), (.~))
import           Data.Default (def)
import           React.Flux

import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon
import Refine.Frontend.Icon.Types
import Refine.Frontend.Screen.Calculations
import Refine.Frontend.Screen.Types
import Refine.Frontend.Store
import Refine.Frontend.Store.Types
import Refine.Frontend.Types
import Refine.Prelude ()


instance UnoverlapAllEq QuickCreateProps

quickCreate :: View '[QuickCreateProps]
quickCreate = mkView "QuickCreateButton" $ \props ->
    case (props ^. quickCreateRange, props ^. quickCreateShowState) of
        (Just range, QuickCreateShown) ->
            iconButton_ $ def @(IconButtonPropsWithHandler QuickCreateSide)
              & iconButtonPropsIconProps    .~ IconProps (renderQuickCreateSide (props ^. quickCreateSide))
                                                         True ("icon-New_Comment", "bright") XXL
              & iconButtonPropsPosition     .~ Just (mkQuickCreateOffset range (props ^. quickCreateScreenState))
              & iconButtonPropsOnClick      .~ (props ^. quickCreateSide)
        _ -> mempty
--    // quickCreate annotation ui events  -- RENAME: annotation => comment
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);

quickCreate_ :: QuickCreateProps -> ReactElementM eventHandler ()
quickCreate_ !props = view_ quickCreate "quickCreate_" props


instance IconButtonPropsOnClick QuickCreateSide where
  runIconButtonPropsOnClick _ _ = dispatch . \case
    QuickCreateComment -> ContributionAction ShowCommentEditor
    QuickCreateEdit    -> HeaderAction ToggleEditToolbarExtension
  defaultOnClick = QuickCreateComment


mkQuickCreateOffset :: Range -> ScreenState -> Int
mkQuickCreateOffset range screenState =
    mkQuickCreateRangeTop range screenState +
    mkQuickCreateRangePos range

-- | This is the offset from the bottom of the toolbar.
mkQuickCreateRangeTop :: Range -> ScreenState -> Int
mkQuickCreateRangeTop range = offsetIntoText
    (offsetFromDocumentTop (range ^. rangeTopOffset) (range ^. rangeScrollOffset))

mkQuickCreateRangePos :: Range -> Int
mkQuickCreateRangePos range = if useIdealCenter then idealCenter else edgePosition
  where
    offsetFromTop   = range ^. rangeDocTopOffset
    selectionHeight = (range ^. rangeBottomOffset - range ^. rangeTopOffset) ^. unOffsetFromViewportTop
    idealCenter     = selectionHeight `div` 2 - 22
    useIdealCenter  = selectionHeight <= 200
    closerToTop     = abs (offsetFromTop ^. unOffsetFromDocumentTop - range ^. rangeTopOffset . unOffsetFromViewportTop) < idealCenter
    edgePosition    = if closerToTop then 0 else selectionHeight - 44
