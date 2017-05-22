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


-- | Variant of 'React.Flux.Combinators.style' with language-css types instead of 'JSString'.
style :: [Decl] -> PropertyOrHandler handler
style = ("style" @=) . declsToJSON

declsToJSON :: [Decl] -> Data.Aeson.Value
declsToJSON = object . map (\(Decl _mprio n a) -> (cs (prettyPrint n) .:= prettyPrint a))


decl :: ToExpr e => Prop -> e -> Decl
decl p e = Decl Nothing p (expr e)

instance IsString Prop where
  fromString = Ident


newtype Rem = Rem Double
  deriving (Eq, Show)

instance ToExpr Rem where
  expr v = expr $ VString (show v <> "rem")  -- FIXME: is this how you do it?


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
