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

module Refine.Frontend.Icon
  ( IconButtonProps
  , icon_, iconButton_
  , IconButtonPropsOnClick(..)
  , defaultIconButtonProps
  ) where

import Refine.Frontend.Prelude
import Language.Css.Syntax

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Store (dispatchMany)
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util


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
                         ]
       , "style" @@= css (props ^. iconPropsSize)
       , onMouseEnter $ \_ _ _ -> ([], Just True)
       , onMouseLeave $ \_ _ _ -> ([], Just False)
       ] mempty

icon_ :: IconProps -> ReactElementM eventHandler ()
icon_ !props = view_ icon "Icon_" props


-- * icon button

iconButtonPropsToClasses :: IconButtonPropsWithHandler onclick -> JSString
iconButtonPropsToClasses props = toClasses $
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

iconButtonPropsToStyles :: IconButtonPropsWithHandler onclick -> [Decl]
iconButtonPropsToStyles props = alpos <> curpoint
  where
    alpos = case props ^. iconButtonPropsPosition of
              Just pos  -> [decl "top" (Px pos)]
              Nothing   -> []
    curpoint = [decl "cursor" (Ident "pointer") | not (props ^. iconButtonPropsDisabled)]

{- TODO we currently ignore touch device handling because there are some issues with
 - browsers emitting tap events on click and we don't know how to handle these properly.

    import Refine.Frontend.ThirdPartyViews (hammer_)
    let bprops = props ^. iconButtonProps
    hammer_ [on "onTap" $ bprops ^. clickHandler | not (bprops ^. disabled)] $ do
-}

iconButton :: IconButtonPropsOnClick onclick => View '[IconButtonPropsWithHandler onclick]
iconButton = mkView "IconButton" $ \props -> do
    div_ ([ "className" $= iconButtonPropsToClasses props
          , "style" @@= iconButtonPropsToStyles props
          ] <> [onClick $ mkClickHandler props | not (props ^. iconButtonPropsDisabled)]
         ) $ do
        icon_ $ props ^. iconButtonPropsIconProps
        span_ [ "className" $= (props ^. iconButtonPropsIconProps . iconPropsBlockName <> "__button-label")
              , "style" @@= [decl "color" Color.DisabledTextColor | props ^. iconButtonPropsDisabled]
              ] $
            elemJSString (props ^. iconButtonPropsLabel)

        -- TODO: i think the span_ node should be a child of the icon_ node so that the css info on
        -- the latter can apply (e.g. "bright" vs. "dark").  a more aggressive refactoring may be a
        -- better idea, though.  this part of the code base is a bit brittle and confusing.

iconButton_ :: IconButtonPropsOnClick onclick => IconButtonPropsWithHandler onclick -> ReactElementM eventHandler ()
iconButton_ !props = view_ iconButton ("iconButton_" <> props ^. iconButtonPropsListKey) props


-- * events

mkClickHandler :: IconButtonPropsOnClick onclick => IconButtonPropsWithHandler onclick -> Event -> MouseEvent -> ViewEventHandler
mkClickHandler props evt mevt =
  (if props ^. iconButtonPropsClickPropag then () else stopPropagation evt) `seq`
  runIconButtonPropsOnClick evt mevt (props ^. iconButtonPropsOnClick)

class (Typeable onclick, Eq onclick) => IconButtonPropsOnClick onclick where  -- TODO: rename to ButtonOnClick
  runIconButtonPropsOnClick :: Event -> MouseEvent -> onclick -> ViewEventHandler
      -- TODO: what do i need the default for again?
  defaultOnClick            :: onclick  -- ^ @instance Default [GlobalAction]@ would lead to overlaps.

instance IconButtonPropsOnClick [GlobalAction] where
  runIconButtonPropsOnClick _ _ = dispatchMany
  defaultOnClick                = mempty

defaultIconButtonProps :: IconButtonPropsOnClick onclick => IconButtonPropsWithHandler onclick
defaultIconButtonProps = IconButtonProps
    { _iconButtonPropsListKey      = ""
    , _iconButtonPropsIconProps    = def
    , _iconButtonPropsElementName  = ""
    , _iconButtonPropsModuleName   = ""
    , _iconButtonPropsLabel        = ""
    , _iconButtonPropsDisabled     = False
    , _iconButtonPropsPosition     = Nothing
    , _iconButtonPropsAlignRight   = False
    , _iconButtonPropsOnClick      = defaultOnClick
    , _iconButtonPropsClickPropag  = True  -- Iff 'False', call 'stopPropagation'.  See 'mkClickHandler'.
    , _iconButtonPropsExtraClasses = []
    }

type IconButtonProps = IconButtonPropsWithHandler [GlobalAction]
