{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Util
where

import Refine.Frontend.Prelude

import qualified Data.Aeson
import qualified Data.JSString as JSS
import Language.Css.Build
import Language.Css.Pretty
import Language.Css.Syntax


class Css a where
  css :: a -> [Decl]

decl :: ToExpr e => Prop -> e -> Decl
decl p e = Decl Nothing p (expr e)

instance IsString Prop where
  fromString = Ident

-- | Like '(@=)', but for css rather than json.
(@@=) :: forall handler. JSString -> [Decl] -> PropertyOrHandler handler
(@@=) k v = k @= declsToJSON v

declsToJSON :: [Decl] -> Data.Aeson.Value
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
  deriving (Eq, Ord, Enum, Show)

instance ToExpr ZIndex where
  expr ZIxDialog   = expr @Int 6050
  expr ZIxOverlay  = expr @Int 6010
  expr ZIxLoginTab = expr @Int 100000

zindex :: ZIndex -> Decl
zindex = decl "zIndex"


toClasses :: (ConvertibleStrings s JSString, ConvertibleStrings JSString s) => [s] -> s
toClasses = cs . JSS.unwords . filter (not . JSS.null) . fmap cs

deriving instance FromJSVal (NoJSONRep JSVal)
deriving instance ToJSVal (NoJSONRep JSVal)

#ifdef __GHCJS__

foreign import javascript unsafe
  "$1 === $2"
  (===) :: JSVal -> JSVal -> Bool

foreign import javascript unsafe
  "$1 !== $2"
  (!==) :: JSVal -> JSVal -> Bool

-- an earlier implementation had two fallbacks:
--
-- ```javascript
--    typeof(target.pageYOffset) === 'number' && target.pageYOffset                 ||
--    document.body                           && document.body.scrollTop            ||
--    document.documentElement                && document.documentElement.scrollTop;
-- ```
foreign import javascript unsafe
  "(function() { return pageYOffset; })()"
  js_getScrollOffset :: IO Int

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

#endif
