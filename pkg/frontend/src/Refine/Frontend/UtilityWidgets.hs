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
  ( IconProps(..), icon_
  , iconPropsBlockName
  , iconPropsHighlight
  , iconPropsDesc
  , iconPropsSize

  , IconButtonProps(..), iconButton_
  , iconButtonPropsListKey
  , iconButtonPropsIconProps
  , iconButtonPropsElementName
  , iconButtonPropsModuleName
  , iconButtonPropsContentType
  , iconButtonPropsLabel
  , iconButtonPropsDisabled
  , iconButtonPropsPosition
  , iconButtonPropsAlignRight
  , iconButtonPropsClickHandler
  , iconButtonPropsExtraClasses

  , IconSize(..)
  , IconDescription
  , ClickHandler
  ) where

import           Control.Lens (makeLenses, (^.), _1, _2)
import           Data.Char (toLower)
import           Data.Default (Default(def))
import           Data.Monoid ((<>))
import           Data.String.Conversions (cs)
import           GHCJS.Types (JSString)
import           React.Flux

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.CS ()
import           Refine.Frontend.Style
import           Refine.Frontend.Util


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


-- * icon

data IconProps = IconProps
  { _iconPropsBlockName :: JSString
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }

type IconDescription = (JSString, JSString)

makeLenses ''IconProps

instance Default IconProps where
  def = IconProps
    { _iconPropsBlockName = ""
    , _iconPropsHighlight = False
    , _iconPropsDesc      = ("", "")
    , _iconPropsSize      = L
    }

icon :: View '[IconProps]
icon = mkStatefulView "Icon" False $ \mouseIsOver props -> do
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


-- * icon button

data IconButtonProps = IconButtonProps
  { _iconButtonPropsListKey      :: ReactListKey  -- (this is not morally part of the props, but it's convenient to keep it here.)
  , _iconButtonPropsIconProps    :: IconProps
  , _iconButtonPropsElementName  :: JSString
  , _iconButtonPropsModuleName   :: JSString
  , _iconButtonPropsContentType  :: JSString
  , _iconButtonPropsLabel        :: JSString
  , _iconButtonPropsDisabled     :: Bool  -- TODO: make this 'enabled'
  , _iconButtonPropsPosition     :: Maybe Int
  , _iconButtonPropsAlignRight   :: Bool
  , _iconButtonPropsClickHandler :: ClickHandler
  , _iconButtonPropsExtraClasses :: [JSString]
  }

type ClickHandler = Event -> [SomeStoreAction]

makeLenses ''IconButtonProps

instance Default IconButtonProps where
  def = IconButtonProps
    { _iconButtonPropsListKey      = ""
    , _iconButtonPropsIconProps    = def
    , _iconButtonPropsElementName  = ""
    , _iconButtonPropsModuleName   = ""
    , _iconButtonPropsContentType  = ""
    , _iconButtonPropsLabel        = ""
    , _iconButtonPropsDisabled     = False
    , _iconButtonPropsPosition     = Nothing
    , _iconButtonPropsAlignRight   = False
    , _iconButtonPropsClickHandler = \_ -> []
    , _iconButtonPropsExtraClasses = []
    }

iconButtonPropsToClasses :: IconButtonProps -> [JSString]
iconButtonPropsToClasses props =
  [ iprops ^. iconPropsBlockName <> "__button"
  , beName  -- for the vdoc-toolbar
  , bemName -- for the buttons in the overlays
  ] <> alignmentClass
    <> props ^. iconButtonPropsExtraClasses
  where
    iprops = props ^. iconButtonPropsIconProps
    beConnector = if props ^. iconButtonPropsElementName == "" then "" else "__"
    emConnector = if props ^. iconButtonPropsModuleName == "" then "" else "--"
    beName  = iprops ^. iconPropsBlockName <> beConnector <> props ^. iconButtonPropsElementName
    bemName = beName <> emConnector <> props ^. iconButtonPropsModuleName
    alignmentClass = [ iprops ^. iconPropsBlockName <> "--align-right" | props ^. iconButtonPropsAlignRight ]

iconButtonPropsToStyles :: IconButtonProps -> [Style]
iconButtonPropsToStyles props = alpos <> curpoint
  where
    alpos = case props ^. iconButtonPropsPosition of
              Just pos  -> [Style "top" pos]
              Nothing   -> []
    curpoint = [Style "cursor" ("pointer" :: String) | not (props ^. iconButtonPropsDisabled)]

{- TODO we currently ignore touch device handling because there are some issues with
 - browsers emitting tap events on click and we don't know how to handle these properly.

    import Refine.Frontend.ThirdPartyViews (hammer_)
    let bprops = props ^. iconButtonProps
    hammer_ [on "onTap" $ bprops ^. clickHandler | not (bprops ^. disabled)] $ do
-}

iconButton :: View '[IconButtonProps]
iconButton = mkView "IconButton" $ \props -> do
    let iprops = props ^. iconButtonPropsIconProps
    div_ ([ "data-content-type" $= (props ^. iconButtonPropsContentType)
          , "className" $= toClasses (iconButtonPropsToClasses props)
          , "style" @= iconButtonPropsToStyles props
          ] <> [onClick $ const . (props ^. iconButtonPropsClickHandler) | not (props ^. iconButtonPropsDisabled)]
          ) $ do
        icon_ iprops
        span_ [ "className" $= (iprops ^. iconPropsBlockName <> "__button-label")
              , "style" @= [Style "color" Color.disabledText | props ^. iconButtonPropsDisabled]
              ] $
            elemJSString (props ^. iconButtonPropsLabel)

iconButton_ :: IconButtonProps -> ReactElementM eventHandler ()
iconButton_ !props = view_ iconButton ("iconButton_" <> props ^. iconButtonPropsListKey) props
