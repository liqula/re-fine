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
