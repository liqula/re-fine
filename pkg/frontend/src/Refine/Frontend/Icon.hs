{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Icon
  ( module Refine.Frontend.Icon.Types
  , ibutton
  , ibutton_
  , sibutton_
  , emptyIbuttonProps
  , IbuttonOnClick(..)
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
  -- to color schema; do not send onClick actions.
  -- when enabled: [ {cursor: pointer} | props ^. ibEnabled ]

  div_ (onMsOvr <> onClk <> ["className" $= toClasses divclss, "style" @@= divstyles]) $ do
    case props ^. ibImage of
      ButtonImageInline (Common.ImageInline i) -> img_ ["src" $= cs i] $ pure ()
      ButtonImageIcon i scm                    -> Svg.render scm (st ^. stlens) i
    forM_ (props ^. ibIndexNum) $
      div_ ["className" $= "number-top-right"] . elemText . cs . show
      -- TODO: index numbers are not scaled to ibSize


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
