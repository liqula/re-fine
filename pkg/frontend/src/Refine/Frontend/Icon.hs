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
  ( module Refine.Frontend.Icon.Types

  , ibutton_
  , emptyIbuttonProps
  , IbuttonOnClick(..)

    -- * outdated
  , icon_, iconButton_
  , IconButtonProps
  , IconButtonPropsOnClick(..)
  , defaultIconButtonProps
  ) where

import Refine.Frontend.Prelude hiding (fn)

import Language.Css.Syntax

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Store (dispatchMany)
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util


-- * icons buttons

ibutton :: IbuttonOnClick onclick => View '[IbuttonProps onclick]
ibutton = mkStatefulView "Ibutton" False $ \mouseIsOver props -> do
  let onMsOvr :: [PropertyOrHandler (StatefulViewEventHandler Bool)]
      onMsOvr = [ onMouseEnter $ \_ _ _ -> ([], Just True)
                , onMouseLeave $ \_ _ _ -> ([], Just False)
                ]

      onClk :: [PropertyOrHandler (StatefulViewEventHandler Bool)]
      onClk = [onClick $ \evt mevt _ -> (mkIbuttonClickHandler props evt mevt, Nothing) | props ^. ibEnabled]

      divSty :: [Decl]
      divSty = [ decl "direction" (Ident "ltr")
               , decl "width" (sizePx $ props ^. ibSize)
               , decl "float" (Ident (case props ^. ibAlign of AlignLeft -> "left"; AlignRight -> "right"))
               , decl "textAlign" (Ident "center")
               , decl "pointerEvents" (Ident "all")
               ] <>
               (-- the float style above only works in main menu, not in the menu bar in the main screen.
                if props ^. ibAlign == AlignRight
                 then [ decl "marginLeft" (Ident "auto")
                      , decl "marginRight" (Percentage 10)
                      ]
                 else [decl "margin" (Px $ sizeInt (props ^. ibSize) `div` 5)])

      iconSty :: [Decl]
      iconSty = [ decl "cursor" (Ident "pointer")
                , decl "borderRadius" (Percentage 100)
                ] <> css (props ^. ibSize)

      bg :: BackgroundImage
      bg = BackgroundImage (props ^. ibImage) imageState
        where
          imageState
            | mouseIsOver && props ^. ibEnabled = BisRO
            | props ^. ibDarkBackground         = BisBright
            | otherwise                         = BisDark

      spanSty :: [Decl]
      spanSty = [ decl "cursor" (Ident "pointer")
                , decl "color" textColor
                , decl "fontSize" (Rem 0.75)
                , decl "marginTop" (Rem 0.3125)
                ]
        where
          textColor
            | not (props ^. ibEnabled)  = Color.DisabledTextColor
            | props ^. ibDarkBackground = Color.TextColorOnDark
            | otherwise                 = Color.TextColor

  div_ (onMsOvr <> onClk <> ["style" @@= divSty]) $ do
    div_  ["style" @@= iconSty, "className" $= iconCssClass bg] $ pure ()
    span_ ["style" @@= spanSty] $ elemText (props ^. ibLabel)

ibutton_ :: IbuttonOnClick onclick => IbuttonProps onclick -> ReactElementM eventHandler ()
ibutton_ props = view_ ibutton ("Ibutton_" <> props ^. ibListKey) props

emptyIbuttonProps :: forall onclick. onclick ~ [GlobalAction] => ST -> onclick -> IbuttonProps onclick
emptyIbuttonProps img onclick = IbuttonProps
  { _ibListKey          = "0"
  , _ibLabel            = "[label]"
  , _ibDarkBackground   = False
  , _ibImage            = img
  , _ibOnClick          = onclick
  , _ibClickPropag      = True
  , _ibEnabled          = True
  , _ibSize             = Large
  , _ibAlign            = AlignLeft
  , _ibPosition         = Nothing
  }


-- * events

class (Typeable onclick, Eq onclick) => IbuttonOnClick onclick where
  runIbuttonOnClick :: Event -> MouseEvent -> onclick -> ViewEventHandler

instance IbuttonOnClick [GlobalAction] where
  runIbuttonOnClick _ _ = dispatchMany

mkIbuttonClickHandler :: IbuttonOnClick onclick => IbuttonProps onclick -> Event -> MouseEvent -> ViewEventHandler
mkIbuttonClickHandler props evt mevt = propag `seq` handle
  where
    propag = if props ^. ibClickPropag then () else stopPropagation evt
    handle = runIbuttonOnClick evt mevt (props ^. ibOnClick)


-- * outdated

-- FUTUREWORK: the rest of this module should be removed and everything ported to the ibutton_
-- component above.  this may take a few more steps, though.


-- ** icon

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


-- ** icon button

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


-- ** events

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
