{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Icon
  ( module Refine.Frontend.Icon.Types

  , ibutton_
  , sibutton_
  , emptyIbuttonProps
  , emptyIbuttonProps_
  , IbuttonOnClick(..)

    -- * outdated
  , icon_, iconButton_
  , IconButtonProps
  , IconButtonPropsOnClick(..)
  , defaultIconButtonProps
  ) where
#include "import_frontend.hs"

import Language.Css.Syntax

import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Store ()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Access


-- * icons buttons

ibutton :: forall onclick. (HasCallStack, IbuttonOnClick onclick 'EventHandlerCode)
        => View '[IbuttonProps onclick]
ibutton = mkStatefulView "Ibutton" False (sibutton_ id)

-- | (implemented in terms of 'sibutton_'.)
--
-- FIXME: there are two kinds of highlight: "mouse-over" and
-- "selected".  we should distinguish those more cleanly and expose a
-- nicer interface to the application.
ibutton_ :: forall onclick handler. (HasCallStack, IbuttonOnClick onclick 'EventHandlerCode)
         => IbuttonProps onclick -> ReactElementM handler ()
ibutton_ props = view_ ibutton ("Ibutton_" <> props ^. ibListKey) props


-- | A variant of 'ibutton_' that inherits the local state from the
-- caller.
--
-- This is not going through the hoops of `mkStatefulView`, because
-- that would insulate the local state from the state the calling
-- component wants to share with this button.  Instead, render the
-- 'ReactElement' directlyk.
sibutton_ :: forall onclick st (handler :: EventHandlerCode *).
                  (HasCallStack, IbuttonOnClick onclick handler, handler ~ 'StatefulEventHandlerCode st)
               => Lens' st Bool -> st -> IbuttonProps onclick -> ReactElementM handler ()
sibutton_ mouseIsOver st props = do
  let --FIXME onMsOvr :: [PropertyOrHandler handler]
      onMsOvr = [ onMouseEnter $ \_ _ -> simpleHandler $ \s -> ([], Just $ s & mouseIsOver .~ True)
                , onMouseLeave $ \_ _ -> simpleHandler $ \s -> ([], Just $ s & mouseIsOver .~ False)
                ]

      --FIXME onClk :: [PropertyOrHandler handler]
      onClk = [onClick $ \evt mevt -> mkIbuttonClickHandler props evt mevt | props ^. ibEnabled]

      -- FIXME: ibutton must not contain divs, so we can use it inside spans.
      divSty :: [Decl]
      divSty = [ decl "direction" (Ident "ltr")
               , decl "width" (maximum [100, sizePx $ props ^. ibSize])
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

      -- FUTUREWORK: do we want to have grayed-out images for all buttons?
      iconSty :: [Decl]
      iconSty = [decl "borderRadius" (Percentage 100)]
             <> [decl "cursor" (Ident "pointer") | props ^. ibEnabled]
             <> css (props ^. ibSize)

      bg :: Either BackgroundImage Common.Image
      bg = case props ^. ibImage of
        ImageInline i -> Right i
        ImageIcon n -> Left $ BackgroundImage n imageState
        where
          imageState = case props ^. ibHighlightWhen of
            HighlightAlways      | props ^. ibEnabled                      -> BisRO
            HighlightOnMouseOver | st ^. mouseIsOver && props ^. ibEnabled -> BisRO
            _                    | props ^. ibDarkBackground               -> BisBright
            _                                                              -> BisDark

      spanSty :: [Decl]
      spanSty = [ decl "color" (Ident textColor)
                , decl "fontSize" (Rem 0.75)
                , decl "marginTop" (Rem 0.3125)
                ]
             <> [decl "cursor" (Ident "pointer") | props ^. ibEnabled]
        where
          textColor
            | not (props ^. ibEnabled)  = "rgba(169, 169, 169, 1)"
            | props ^. ibDarkBackground = "rgba(210, 217, 223, 1)"
            | otherwise                 = "rgba(0, 0, 0, 1)"

  div_ (onMsOvr <> onClk <> ["style" @@= divSty]) $ do
    case bg of
      Left i  -> div_  ["style" @@= iconSty, "className" $= iconCssClass i] $ pure ()
      Right (Common.Image i) -> img_ ["src" $= cs i] $ pure ()
    span_ ["style" @@= spanSty] $ elemText (props ^. ibLabel)


emptyIbuttonProps :: HasCallStack => forall onclick. ST -> onclick -> IbuttonProps onclick
emptyIbuttonProps = emptyIbuttonProps_ . ImageIcon

emptyIbuttonProps_ :: HasCallStack => forall onclick. ButtonImage -> onclick -> IbuttonProps onclick
emptyIbuttonProps_ img onclick = IbuttonProps
  { _ibListKey          = "0"
  , _ibLabel            = mempty
  , _ibDarkBackground   = False
  , _ibImage            = img
  , _ibHighlightWhen    = HighlightOnMouseOver
  , _ibOnClick          = onclick
  , _ibOnClickMods      = []
  , _ibEnabled          = True
  , _ibSize             = Large
  , _ibAlign            = AlignLeft
  }


-- * events

instance IbuttonOnClick [GlobalAction] 'EventHandlerCode where
  runIbuttonOnClick _ _ = mconcat . fmap dispatch

instance IbuttonOnClick [AccessAction] 'EventHandlerCode where
  runIbuttonOnClick _ _ = mconcat . fmap dispatch

instance {-# OVERLAPPABLE #-} IbuttonOnClick action 'EventHandlerCode => IbuttonOnClick action ('StatefulEventHandlerCode st) where
  runIbuttonOnClick evt mevt onclick _st = (runIbuttonOnClick evt mevt onclick, Nothing)

instance IbuttonOnClick action ('StatefulEventHandlerCode st) => IbuttonOnClick action ('StatefulEventHandlerCode (IbuttonState st)) where
  runIbuttonOnClick evt mevt onclick (IbuttonState mo st) =
    case runIbuttonOnClick evt mevt onclick st of
      (act, mupd) -> (act, IbuttonState mo <$> mupd)

-- | Handle stopPropagation etc., then run the 'runIbuttonOnClick' from the matching instance.
mkIbuttonClickHandler :: (HasCallStack, IbuttonOnClick onclick handler)
                      => IbuttonProps onclick -> Event -> MouseEvent -> (EventHandlerType handler, [EventModification])
mkIbuttonClickHandler props evt mevt = propag handle
  where
    propag = case props ^. ibOnClickMods of
     []                -> simpleHandler
     [PreventDefault]  -> preventDefault
     [StopPropagation] -> stopPropagation
     bad               -> error $ "ibutton_: bad combination of click event mods: " <> show bad
    handle = runIbuttonOnClick evt mevt (props ^. ibOnClick)


-- * outdated

-- FUTUREWORK: the rest of this module should be removed and everything ported to the ibutton_
-- component above.  this may take a few more steps, though.


-- ** icon

icon :: HasCallStack => View '[IconProps]
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
       , "style" @@= (css (props ^. iconPropsSize) <> [decl "marginRight" (Px 15)])
       , onMouseEnter $ \_ _ -> simpleHandler $ \_ -> ([], Just True)
       , onMouseLeave $ \_ _ -> simpleHandler $ \_ -> ([], Just False)
       ] mempty

icon_ :: HasCallStack => IconProps -> ReactElementM eventHandler ()
icon_ = view_ icon "Icon_"


-- ** icon button

iconButtonPropsToClasses :: HasCallStack => IconButtonPropsWithHandler onclick -> JSString
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

iconButtonPropsToStyles :: HasCallStack => IconButtonPropsWithHandler onclick -> [Decl]
iconButtonPropsToStyles props = alpos <> curpoint
  where
    alpos = case props ^. iconButtonPropsPosition of
              Just pos  -> [decl "top" (Px pos)]
              Nothing   -> []
    curpoint = [decl "cursor" (Ident "pointer") | not (props ^. iconButtonPropsDisabled)]

{- FIXME: we currently ignore touch device handling because there are some issues with
   browsers emitting tap events on click and we don't know how to handle these properly.

    import Refine.Frontend.ThirdPartyViews (hammer_)
    let bprops = props ^. iconButtonProps
    hammer_ [on "onTap" $ bprops ^. clickHandler | not (bprops ^. disabled)] $ do
-}

iconButton :: HasCallStack => IconButtonPropsOnClick onclick => View '[IconButtonPropsWithHandler onclick]
iconButton = mkView "IconButton" $ \props -> do
    div_ ([ "className" $= iconButtonPropsToClasses props
          , "style" @@= iconButtonPropsToStyles props
          ] <> [onClick $ mkClickHandler props | not (props ^. iconButtonPropsDisabled)]
         ) $ do
        icon_ $ props ^. iconButtonPropsIconProps
        span_ [ "className" $= (props ^. iconButtonPropsIconProps . iconPropsBlockName <> "__button-label")
              , "style" @@= ([decl "color" (Ident "rgba(169, 169, 169, 1)") | props ^. iconButtonPropsDisabled] <>
                             [decl "marginRight" (Px 15)])
              ] $
            elemJSString (props ^. iconButtonPropsLabel)

        -- FIXME: i think the span_ node should be a child of the icon_ node so that the css info on
        -- the latter can apply (e.g. "bright" vs. "dark").  a more aggressive refactoring may be a
        -- better idea, though.  this part of the code base is a bit brittle and confusing.

iconButton_ :: HasCallStack => IconButtonPropsOnClick onclick => IconButtonPropsWithHandler onclick -> ReactElementM eventHandler ()
iconButton_ props = view_ iconButton ("iconButton_" <> props ^. iconButtonPropsListKey) props


-- ** events

mkClickHandler :: HasCallStack => IconButtonPropsOnClick onclick => IconButtonPropsWithHandler onclick -> Event -> MouseEvent -> (ViewEventHandler, [EventModification])
mkClickHandler props evt mevt =
  (case props ^. iconButtonPropsOnClickMods of
     []                -> simpleHandler
     [PreventDefault]  -> preventDefault
     [StopPropagation] -> stopPropagation
     bad               -> error $ "ibutton_: bad combination of click event mods: " <> show bad) $
  runIconButtonPropsOnClick evt mevt (props ^. iconButtonPropsOnClick)

class (Typeable onclick, Eq onclick) => IconButtonPropsOnClick onclick where  -- FIXME: rename to ButtonOnClick
  runIconButtonPropsOnClick :: Event -> MouseEvent -> onclick -> ViewEventHandler
      -- FIXME: what do i need the default for again?
  defaultOnClick            :: onclick  -- ^ @instance Default [GlobalAction]@ would lead to overlaps.

instance IconButtonPropsOnClick [GlobalAction] where
  runIconButtonPropsOnClick _ _ = mconcat . fmap dispatch
  defaultOnClick                = mempty

instance IconButtonPropsOnClick [AccessAction] where
  runIconButtonPropsOnClick _ _ = mconcat . fmap dispatch
  defaultOnClick                = mempty

defaultIconButtonProps :: HasCallStack => IconButtonPropsOnClick onclick => IconButtonPropsWithHandler onclick
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
    , _iconButtonPropsOnClickMods  = []  -- See 'mkClickHandler'.
    , _iconButtonPropsExtraClasses = []
    }

type IconButtonProps = IconButtonPropsWithHandler [GlobalAction]
