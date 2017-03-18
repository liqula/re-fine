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

module Refine.Frontend.UtilityWidgets
( IconSize(..)
, IconDescription
, IconProps(..)
, ClickHandler
, IconButtonProps(..)
, IconButtonWithAlignmentProps(IconButtonWithAlignmentProps)

, icon_
, iconButtonWithAlignment_
, iconButtonWithAlignmentCore_
, iconButton_
, positionedIconButton_

, toClasses
) where

import           Control.Lens (makeLenses, (^.), _1, _2)
import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.String.Conversions (cs)
import           GHCJS.Types (JSString)
import           React.Flux

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Style
import           Refine.Prelude()
-- TODO import           Refine.Frontend.ThirdPartyViews (hammer_)


data IconSize
    = S
    | M
    | L
    | XL
    | XXL
    deriving Show

type IconDescription = (String, String)

data IconProps = IconProps
  { _iconPropsBlockName :: String
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }

makeLenses ''IconProps

type ClickHandler = Event -> [SomeStoreAction]

data IconButtonProps = IconButtonProps
  { _iconButtonPropsIconProps    :: IconProps
  , _iconButtonPropsElementName  :: String
  , _iconButtonPropsModuleName   :: String
  , _iconButtonPropsContentType  :: JSString
  , _iconButtonPropsLabel        :: JSString
  , _iconButtonPropsDisabled     :: Bool
  , _iconButtonPropsClickHandler :: ClickHandler
  , _iconButtonPropsExtraClasses :: [String]
  }

makeLenses ''IconButtonProps

data IconButtonWithAlignmentProps = IconButtonWithAlignmentProps
  { _iconButtonWithAlIconButtonProps :: IconButtonProps
  , _iconButtonWithAlRightAligned    :: Bool
  , _iconButtonWithAlPosition        :: Maybe Int
  }

makeLenses ''IconButtonWithAlignmentProps


icon :: View '[IconProps]
icon = mkStatefulView "Icon" False $ \mouseIsOver props -> do
  -- TODO unify the naming schemas of the classes of the different icons!
  let
    highlightStyle = if mouseIsOver && (props ^. iconPropsHighlight)
                     then "RO"
                     else props ^. iconPropsDesc . _2
  div_ ["className" $= (cs . toClasses)
                         [ (props ^. iconPropsBlockName) <> "__icon"
                         , if props ^. iconPropsHighlight then "o-icon-highlight" else ""
                         , props ^. iconPropsDesc . _1 <> "_" <> highlightStyle
                         , "iconsize-" <> map toLower (show (props ^. iconPropsSize))
                         ]
       , onMouseEnter $ \_ _ _ -> ([], Just True)
       , onMouseLeave $ \_ _ _ -> ([], Just False)
       ] mempty

icon_ :: IconProps -> ReactElementM eventHandler ()
icon_ !props = view_ icon "Icon_" props


iconButtonWithAlignment :: View '[IconButtonWithAlignmentProps]
iconButtonWithAlignment = mkView "IconButtonWithAlignment" $ \props -> do
{- TODO we currently ignore touch device handling because there are some issues with
 - browsers emitting tap events on click and we don't know how to handle these properly.

    let bprops = props ^. iconButtonProps
    hammer_ [on "onTap" $ bprops ^. clickHandler | not (bprops ^. disabled)] $ do
-}
      iconButtonWithAlignmentCore_ props

iconButtonWithAlignment_ :: IconButtonWithAlignmentProps -> ReactElementM eventHandler ()
iconButtonWithAlignment_ !props = view_ iconButtonWithAlignment "iconButtonWithAlignment_" props

iconButtonWithAlignmentCore :: View '[IconButtonWithAlignmentProps]
iconButtonWithAlignmentCore = mkView "IconButtonWithAlignmentCore" $ \props -> do
    let bprops = props ^. iconButtonWithAlIconButtonProps
    let iprops = bprops ^. iconButtonPropsIconProps
    let beConnector = if bprops ^. iconButtonPropsElementName == "" then "" else "__"
    let emConnector = if bprops ^. iconButtonPropsModuleName == "" then "" else "--"
    let beName  = iprops ^. iconPropsBlockName <> beConnector <> bprops ^. iconButtonPropsElementName
    let bemName = beName <> emConnector <> bprops ^. iconButtonPropsModuleName
    div_ ([ "data-content-type" $= (bprops ^. iconButtonPropsContentType)
           -- TODO unify the naming schema of the classes for the different buttons!
          , "className" $= cs (toClasses $ [ iprops ^. iconPropsBlockName <> "__button"
                                           , beName  -- for the vdoc-toolbar
                                           , bemName -- for the buttons in the overlays
                                           , alignmentClass (iprops ^. iconPropsBlockName)
                                                            (props ^. iconButtonWithAlRightAligned)
                                           ] <> bprops ^. iconButtonPropsExtraClasses)
          , "style" @= (case props ^. iconButtonWithAlPosition of
                               Nothing  -> []
                               Just pos -> [Style "top" pos]
                    <> [Style "cursor" ("pointer" :: String) | not (bprops ^. iconButtonPropsDisabled)])
          ] <> [onClick $ const . (bprops ^. iconButtonPropsClickHandler) | not (bprops ^. iconButtonPropsDisabled)]
          ) $ do
        icon_ iprops
        span_ ["className" $= cs (iprops ^. iconPropsBlockName <> "__button-label")
              , "style" @= [Style "color" Color.disabledText | bprops ^. iconButtonPropsDisabled]
              ] $
            elemJSString (bprops ^. iconButtonPropsLabel)
    where
      alignmentClass blockName1 rightAligned1 = if rightAligned1 then blockName1 <> "--align-right" else ""

iconButtonWithAlignmentCore_ :: IconButtonWithAlignmentProps -> ReactElementM eventHandler ()
iconButtonWithAlignmentCore_ !props = view_ iconButtonWithAlignmentCore "iconButtonWithAlignmentCore_" props

iconButton :: View '[IconButtonProps]
iconButton = mkView "IconButton" $ \props ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned1 Nothing
    where rightAligned1 = False -- no right alignment in the standard case

iconButton_ :: IconButtonProps -> ReactElementM eventHandler ()
iconButton_ !props = view_ iconButton "iconButton_" props

positionedIconButton :: View '[(IconButtonProps, Int)]
positionedIconButton = mkView "IconButton" $ \(props, position1) ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned1 (Just position1)
    where rightAligned1 = False -- no right alignment in the standard case

positionedIconButton_ :: (IconButtonProps, Int) -> ReactElementM eventHandler ()
positionedIconButton_ !props = view_ positionedIconButton "positionedIconButton_" props


toClasses :: [String] -> String
toClasses = unwords . compact
  where
    compact :: [String] -> [String]
    compact = filter $ not . null
