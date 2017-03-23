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
) where

import           Control.Lens (makeLenses, (^.), _1, _2)
import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.String.Conversions (cs)
import           GHCJS.Types (JSString)
import           React.Flux

import           Refine.Frontend.Util
import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Style
import           Refine.Frontend.CS ()
-- TODO import           Refine.Frontend.ThirdPartyViews (hammer_)


type ReactListKey = JSString  -- do not move this to Frontend.Types, importing this here creates a cycle.


data IconSize
    = S
    | M
    | L
    | XL
    | XXL
    deriving Show

instance CssClass IconSize where
  showCssClass = ("iconsize-" <>) . cs . fmap toLower . show

type IconDescription = (JSString, JSString)

data IconProps = IconProps
  { _iconPropsBlockName :: JSString
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }

makeLenses ''IconProps

type ClickHandler = Event -> [SomeStoreAction]

data IconButtonProps = IconButtonProps
  { _iconButtonPropsListKey      :: ReactListKey
  , _iconButtonPropsIconProps    :: IconProps
  , _iconButtonPropsElementName  :: JSString
  , _iconButtonPropsModuleName   :: JSString
  , _iconButtonPropsContentType  :: JSString
  , _iconButtonPropsLabel        :: JSString
  , _iconButtonPropsDisabled     :: Bool
  , _iconButtonPropsClickHandler :: ClickHandler
  , _iconButtonPropsExtraClasses :: [JSString]
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
  div_ ["className" $= toClasses
                         [ (props ^. iconPropsBlockName) <> "__icon"
                         , if props ^. iconPropsHighlight then "o-icon-highlight" else ""
                         , props ^. iconPropsDesc . _1 <> "_" <> highlightStyle
                         , showCssClass (props ^. iconPropsSize)
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
    div_ ([ "key" $= (bprops ^. iconButtonPropsListKey)  -- TODO: append this to the first argument of 'mkView' instead!
          , "data-content-type" $= (bprops ^. iconButtonPropsContentType)
          , "className" $= toClasses (iconButtonPropsToClasses props)
          , "style" @= iconButtonPropsToStyles props
          ] <> [onClick $ const . (bprops ^. iconButtonPropsClickHandler) | not (bprops ^. iconButtonPropsDisabled)]
          ) $ do
        icon_ iprops
        span_ ["className" $= (iprops ^. iconPropsBlockName <> "__button-label")
              , "style" @= [Style "color" Color.disabledText | bprops ^. iconButtonPropsDisabled]
              ] $
            elemJSString (bprops ^. iconButtonPropsLabel)

iconButtonPropsToClasses :: IconButtonWithAlignmentProps -> [JSString]
iconButtonPropsToClasses props =
  [ iprops ^. iconPropsBlockName <> "__button"
  , beName  -- for the vdoc-toolbar
  , bemName -- for the buttons in the overlays
  , alignmentClass (iprops ^. iconPropsBlockName) (props ^. iconButtonWithAlRightAligned)
  ] <> bprops ^. iconButtonPropsExtraClasses
  where
    bprops = props ^. iconButtonWithAlIconButtonProps
    iprops = bprops ^. iconButtonPropsIconProps
    beConnector = if bprops ^. iconButtonPropsElementName == "" then "" else "__"
    emConnector = if bprops ^. iconButtonPropsModuleName == "" then "" else "--"
    beName  = iprops ^. iconPropsBlockName <> beConnector <> bprops ^. iconButtonPropsElementName
    bemName = beName <> emConnector <> bprops ^. iconButtonPropsModuleName
    alignmentClass blockName1 rightAligned1 = if rightAligned1 then blockName1 <> "--align-right" else ""

iconButtonPropsToStyles :: IconButtonWithAlignmentProps -> [Style]
iconButtonPropsToStyles props = alpos <> curpoint
  where
    alpos = case props ^. iconButtonWithAlPosition of
              Nothing  -> []
              Just pos -> [Style "top" pos]
    curpoint = [Style "cursor" ("pointer" :: String) | not (props ^. iconButtonWithAlIconButtonProps . iconButtonPropsDisabled)]

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
