{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Icon
  ( module Refine.Frontend.Icon.Types

  , ibutton_
  , sibutton_
  , emptyIbuttonProps
  , IbuttonOnClick(..)

    -- * outdated
  , icon_, iconButton_
  , IconButtonProps
  , IconButtonPropsOnClick(..)
  , defaultIconButtonProps
  ) where
#include "import_frontend.hs"

import Language.Css.Syntax

import           Refine.Frontend.Access
import           Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Store ()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util


-- * icons buttons

ibutton :: forall onclick. (HasCallStack, IbuttonOnClick onclick 'EventHandlerCode)
        => Maybe Bool -> View '[IbuttonProps onclick]
ibutton pressed = mkStatefulView "Ibutton" (ButtonState (f pressed) NotRollOver) (sibutton_ id)
  where
    f Nothing      = Released
    f (Just False) = Released
    f (Just True)  = Pressed

-- | (implemented in terms of 'sibutton_'.)
--
-- FIXME: there are two kinds of highlight: "mouse-over" and
-- "selected".  we should distinguish those more cleanly and expose a
-- nicer interface to the application.
ibutton_ :: forall onclick handler. (HasCallStack, IbuttonOnClick onclick 'EventHandlerCode)
         => IbuttonProps onclick -> ReactElementM handler ()
ibutton_ props = view_ (ibutton (props ^. ibPressed)) ("Ibutton_" <> props ^. ibListKey) props


-- | A variant of 'ibutton_' that inherits the local state from the
-- caller.
--
-- This is not going through the hoops of `mkStatefulView`, because
-- that would insulate the local state from the state the calling
-- component wants to share with this button.  Instead, render the
-- 'ReactElement' directly.
sibutton_ :: forall onclick st (handler :: EventHandlerCode *).
                  (HasCallStack, IbuttonOnClick onclick handler, handler ~ 'StatefulEventHandlerCode st)
               => Lens' st ButtonState -> st -> IbuttonProps onclick -> ReactElementM handler ()
sibutton_ stlens st props = do
  let onMsOvr :: [PropertyOrHandler handler]
      onMsOvr = [ onMouseEnter $ \_ _ -> simpleHandler $ \s -> ([], Just $ s & stlens . buttonRollOver .~ RollOver)
                , onMouseLeave $ \_ _ -> simpleHandler $ \s -> ([], Just $ s & stlens . buttonRollOver .~ NotRollOver)
                ]

      onClk :: [PropertyOrHandler handler]
      onClk = [onClick $ \evt mevt -> mkIbuttonClickHandler props evt mevt | props ^. ibEnabled]

      divclss = [ iconSizeCls (props ^. ibSize)
                , "margin1"  -- only sometimes; introduce 'ibExtraClasses' for this.
                ]

      -- TODO: should this go to scss for coherence reasons?
      divstyles = [decl "pointerEvents" (Ident "all"), decl "cursor" (Ident "pointer")]

  -- TODO: if pressable (according to props), toggle pressed state.

  -- TODO: when disabled: set opacity to 50% (for both img tag and inline-svg); do not send rollover
  -- to color schema.  when enabled: [ {cursor: pointer} | props ^. ibEnabled ]

  div_ (onMsOvr <> onClk <> ["className" $= toClasses divclss, "style" @@= divstyles]) $ do
    case props ^. ibImage of
      ButtonImageInline (Common.ImageInline i) -> img_ ["src" $= cs i] $ pure ()
      ButtonImageIcon i scm                    -> Svg.render scm (st ^. stlens) i
    forM_ (props ^. ibIndexNum) $
      div_ ["className" $= "number-top-right"] . elemText . cs . show


emptyIbuttonProps :: HasCallStack => forall onclick. ButtonImage -> onclick -> IbuttonProps onclick
emptyIbuttonProps img onclick = IbuttonProps
  { _ibListKey          = "0"
  , _ibOnClick          = onclick
  , _ibOnClickMods      = []
  , _ibPressed          = Nothing
  , _ibImage            = img
  , _ibIndexNum         = Nothing
  , _ibEnabled          = True
  , _ibSize             = Large
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
