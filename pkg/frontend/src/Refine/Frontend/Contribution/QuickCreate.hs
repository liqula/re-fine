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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Contribution.QuickCreate where

import           Control.Lens ((^.), (&), (.~))
import           Data.Default (def)
import           Data.Monoid ((<>))
import           Data.String.Conversions (cs)
import           React.Flux

import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Screen.Calculations
import Refine.Frontend.Screen.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Icon
import Refine.Frontend.Icon.Types
import Refine.Prelude ()

quickCreate :: View '[QuickCreateProps]
quickCreate = mkView "QuickCreateButton" $ \(QuickCreateProps (cs -> createType) currentSelection screenState displayInfo) ->
  if displayInfo == CommentToolbarExtensionWithSelection then mempty -- do not display the buttons when selection was activated via toolbar
  else
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        RangeSelected range offsetFromTop ->
            iconButton_ $ def
              & iconButtonPropsIconProps    .~ IconProps ("o-add-" <> createType) True ("icon-New_Comment", "bright") XXL
              & iconButtonPropsPosition     .~ Just (quickCreateOffset range offsetFromTop screenState)
              & iconButtonPropsClickActions .~ [ContributionAction . ShowCommentEditor $ Just range]
        _ -> mempty
--    // quickCreate annotation ui events  -- RENAME: annotation => comment
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


quickCreateOffset :: Range -> OffsetFromDocumentTop -> ScreenState -> Int
quickCreateOffset range offsetFromTop screenState =
    quickCreateSelectionTop range screenState +
    quickCreateSelectionPos range offsetFromTop

-- | This is the offset from the bottom of the toolbar.
quickCreateSelectionTop :: Range -> ScreenState -> Int
quickCreateSelectionTop range = offsetIntoText
  (offsetFromDocumentTop (range ^. rangeTopOffset) (range ^. rangeScrollOffset))
      -- FIXME: should Range contain an OffsetFromDocumentTop instead?

quickCreateSelectionPos :: Range -> OffsetFromDocumentTop -> Int
quickCreateSelectionPos range offsetFromTop =
    let selectionHeight = (range ^. rangeBottomOffset - range ^. rangeTopOffset) ^. unOffsetFromViewportTop
        idealCenter     = selectionHeight `div` 2 - 22
        useIdealCenter  = selectionHeight <= 200
        closerToTop     = abs (offsetFromTop ^. unOffsetFromDocumentTop - range ^. rangeTopOffset . unOffsetFromViewportTop) < idealCenter
        edgePosition    = if closerToTop then 0 else selectionHeight - 44
    in if useIdealCenter then idealCenter else edgePosition

data QuickCreateProps = QuickCreateProps
  { _quickCreateContributionkind :: String  -- FIXME: use the proper data type here (or, more likely, get rid of this field.)
  , _quickCreateRange            :: Selection
  , _quickCreateOffset           :: ScreenState
  , _quickCreateInfo             :: ToolbarExtensionStatus
  }
  deriving (Eq)

-- "annotation" (RENAME: Comment), "modification" (RENAME: Edit)
quickCreate_ :: QuickCreateProps -> ReactElementM eventHandler ()
quickCreate_ !props = view_ quickCreate "quickCreate_" props
