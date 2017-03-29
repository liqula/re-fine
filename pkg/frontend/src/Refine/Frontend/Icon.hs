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

module Refine.Frontend.Icon
  ( icon_, iconButton_
  ) where

import           Control.Lens ((^.), _1, _2)
import           Data.Monoid ((<>))
import           GHCJS.Types (JSString)
import           React.Flux

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Store
import           Refine.Frontend.Style
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Icon.Types


-- * icon

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
              Just pos  -> [StyleInt "top" pos]
              Nothing   -> []
    curpoint = [StyleST "cursor" "pointer" | not (props ^. iconButtonPropsDisabled)]

{- TODO we currently ignore touch device handling because there are some issues with
 - browsers emitting tap events on click and we don't know how to handle these properly.

    import Refine.Frontend.ThirdPartyViews (hammer_)
    let bprops = props ^. iconButtonProps
    hammer_ [on "onTap" $ bprops ^. clickHandler | not (bprops ^. disabled)] $ do
-}

iconButton :: View '[IconButtonProps]
iconButton = mkView "IconButton" $ \props -> do
    let iprops = props ^. iconButtonPropsIconProps

    div_ ([ "className" $= toClasses (iconButtonPropsToClasses props)
          , "style" @= iconButtonPropsToStyles props
          ] <> [onClick $ mkClickHandler props | not (props ^. iconButtonPropsDisabled)]
          ) $ do
        icon_ iprops
        span_ [ "className" $= (iprops ^. iconPropsBlockName <> "__button-label")
              , "style" @= [mkStyle "color" Color.DisabledText | props ^. iconButtonPropsDisabled]
              ] $
            elemJSString (props ^. iconButtonPropsLabel)

iconButton_ :: IconButtonProps -> ReactElementM eventHandler ()
iconButton_ !props = view_ iconButton ("iconButton_" <> props ^. iconButtonPropsListKey) props

mkClickHandler :: IconButtonProps -> Event -> MouseEvent -> [SomeStoreAction]
mkClickHandler props evt _ = [ stopPropagation evt | not $ props ^. iconButtonPropsClickPropag ]
                          <> dispatchMany (props ^. iconButtonPropsClickActions)
