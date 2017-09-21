{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Util where
#include "import_frontend.hs"

import Data.List (groupBy)

import Language.Css.Build
import Language.Css.Pretty
import Language.Css.Syntax


class Css a where
  css :: a -> [Decl]

-- group all occurances of one prop, separate all values by spaces.
instance Css [Decl] where
  css = map accum . grp
    where
      grp :: [Decl] -> [[Decl]]
      grp = groupBy ((==) `on` f) . sortBy (compare `on` f)
        where
          f (Decl prio prop _) = show (prop, prio)

      accum :: [Decl] -> Decl
      accum (Decl prio prop e: Decl _ _ e': xs) = accum (Decl prio prop (SpaceSep e e'): xs)
      accum [d] = d
      accum [] = error "impossible."


-- | FIXME: remove this in favor of '(:::)'
decl :: HasCallStack => ToExpr e => Prop -> e -> Decl
decl p e = Decl Nothing p (expr e)

(||=) :: (ToExpr e) => Prop -> e -> Decl
(||=) = decl


instance IsString Prop where
  fromString = Ident

-- | Like '(@=)', but for css rather than json.
-- FIXME: remove this in favor of 'style'
(@@=) :: HasCallStack => forall handler. JSString -> [Decl] -> PropertyOrHandler handler
(@@=) k v = k @= declsToJSON v

style :: [Decl] -> PropertyOrHandler h
style = ("style" @@=)


declsToJSON :: HasCallStack => [Decl] -> Aeson.Value
declsToJSON = object . map (\(Decl _mprio n a) -> (cs (prettyPrint n) .:= prettyPrint a))


newtype Rem = Rem Double
  deriving (Eq, Show)

instance ToExpr Rem where
  expr (Rem d) = expr (Ident (show d <> "rem"))  -- FIXME: #317

instance Num Px where  -- FIXME: #317  (also, Px should be a newtype)
  Px i + Px k = Px (i + k)
  Px i - Px k = Px (i - k)
  Px i * Px k = Px (i * k)
  abs (Px i) = Px (abs i)
  signum (Px i) = Px (signum i)
  fromInteger = Px . fromInteger

instance Enum Px where
  fromEnum (Px i) = i
  toEnum = Px

instance Ord Px where
  compare (Px i) (Px j) = compare i j


{- | FIXME: move all of the css z-index handling here.  currently, grep shows this:

pkg/frontend/scss/0-settings/_settings.components.scss:3:// z-index
pkg/frontend/scss/2-generic/_generic.pagelayout.scss:37:            z-index: -1;
pkg/frontend/scss/2-generic/_generic.pagelayout.scss:41:            z-index: -1;
pkg/frontend/scss/4-components/_components.mainmenu-content.scss:23:    z-index: $always-on-top;
pkg/frontend/scss/4-components/_components.mainmenu.scss:19:    z-index: $mainmenu-button-index;
pkg/frontend/scss/4-components/_components.mainmenu.scss:20:    //z-index: $always-on-top;
pkg/frontend/scss/4-components/_components.mainmenu.scss:28:        z-index: $mainmenu-bg2-index;
pkg/frontend/scss/4-components/_components.mainmenu.scss:52:        z-index: $always-on-top;
pkg/frontend/scss/4-components/_components.vdoc-header.scss:42:            z-index: 100;
pkg/frontend/scss/4-components/_components.vdoc-toolbar-extension.scss:39:        z-index: 100200;
pkg/frontend/scss/4-components/_components.vdoc-toolbar.scss:14:    //z-index: $vdoc-toolbar-index;
pkg/frontend/scss/4-components/_components.vdoc-toolbar.scss:50:                z-index: -1;
pkg/frontend/scss/4-components/_components.vdoc-toolbar.scss:83:        z-index: $toolbar-icons-index;
pkg/frontend/scss/5-overrides/_overrides.debug.scss:8:    z-index: $always-on-top + 100;
pkg/frontend/scss/main.scss:20:  z-index: $fullheader-index;
pkg/frontend/scss/main.scss:25:  z-index: $toolbar-index;

once these are removed, we can do this:

instance ToExpr ZIndex where expr = expr . fromEnum

-}
data ZIndex =
    ZIxDialog
  | ZIxOverlay
  | ZIxLoginTab
  | ZIxArticle
  deriving (Eq, Ord, Enum, Show)

instance ToExpr ZIndex where
  expr ZIxDialog   = expr @Int 6050
  expr ZIxOverlay  = expr @Int 6010
  expr ZIxLoginTab = expr @Int 100000
  expr ZIxArticle  = expr @Int 2000  -- ^ must be lower than 4000 to cover @.c-fulltoolbar@

zindex :: HasCallStack => ZIndex -> Decl
zindex = decl "zIndex"


toClasses :: HasCallStack => (ConvertibleStrings s JSString) => [s] -> JSString
toClasses = JSS.unwords . filter (not . JSS.null) . fmap cs

deriving instance FromJSVal (NoJSONRep JSVal)
deriving instance ToJSVal (NoJSONRep JSVal)

-- | use this instead of inlined strings in case of cache misses!
hourglass :: IsString s => s
hourglass = "loading..."


#ifdef __GHCJS__

foreign import javascript safe
  "$1 === $2"
  (===) :: JSVal -> JSVal -> Bool

foreign import javascript safe
  "$1 !== $2"
  (!==) :: JSVal -> JSVal -> Bool

-- an earlier implementation had two fallbacks:
--
-- ```javascript
--    typeof(target.pageYOffset) === 'number' && target.pageYOffset                 ||
--    document.body                           && document.body.scrollTop            ||
--    document.documentElement                && document.documentElement.scrollTop;
-- ```
foreign import javascript safe
  "$r = pageYOffset"
  js_getScrollOffset :: IO Int

-- FUTUREWORK: to make this smoother, check out
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView (but
-- https://bugs.chromium.org/p/chromium/issues/detail?id=648446);
-- https://github.com/bySabi/react-scrollchor;
-- https://stackoverflow.com/questions/30495062/how-can-i-scroll-a-div-to-be-visible-in-reactjs#30497101;
-- https://github.com/sitepoint-editors/smooth-scrolling
foreign import javascript safe
  "scrollBy(0, -pageYOffset)"
  js_scrollToPageTop :: IO ()

-- see js_scrollToPageTop
foreign import javascript safe
  "scrollBy(0, $1-pageYOffset)"
  js_scrollToPx :: Int -> IO ()

-- this scrolls block key into the vertical center of the screen.  (works best if toolbar is a bit
-- shorter than half the screen height.)
foreign import javascript safe
  "scrollBy(0, document.querySelector(\"span[data-offset-key='\" + $1 + \"-0-0']\").getBoundingClientRect().top - (innerHeight / 2))"
  js_scrollToBlockKey :: JSString -> IO ()

#else

{-# ANN (===) ("HLint: ignore Use camelCase" :: String) #-}
(===) :: JSVal -> JSVal -> Bool
(===) = error "javascript FFI not available in GHC"

{-# ANN (!==) ("HLint: ignore Use camelCase" :: String) #-}
(!==) :: JSVal -> JSVal -> Bool
(!==) = error "javascript FFI not available in GHC"

{-# ANN js_getScrollOffset ("HLint: ignore Use camelCase" :: String) #-}
js_getScrollOffset :: IO Int
js_getScrollOffset = error "javascript FFI not available in GHC"

{-# ANN js_scrollToPageTop ("HLint: ignore Use camelCase" :: String) #-}
js_scrollToPageTop :: IO ()
js_scrollToPageTop = error "javascript FFI not available in GHC"

{-# ANN js_scrollToPx ("HLint: ignore Use camelCase" :: String) #-}
js_scrollToPx :: Int -> IO ()
js_scrollToPx = error "javascript FFI not available in GHC"

{-# ANN js_scrollToBlockKey ("HLint: ignore Use camelCase" :: String) #-}
js_scrollToBlockKey :: JSString -> IO ()
js_scrollToBlockKey = error "javascript FFI not available in GHC"

#endif
