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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Util
where

import Refine.Frontend.Prelude

import qualified Data.JSString as JSS
import           Data.String.Conversions
import           GHCJS.Marshal
import           GHCJS.Types (JSVal)
import           React.Flux

import           Refine.Frontend.CS ()
import           Refine.Frontend.Types
import           Refine.Prelude.Aeson (NoJSONRep(..))


toClasses :: (ConvertibleStrings s JSS.JSString, ConvertibleStrings JSS.JSString s) => [s] -> s
toClasses = cs . JSS.unwords . filter (not . JSS.null) . fmap cs

attrToProp :: Attr -> forall handler. PropertyOrHandler handler
attrToProp (Attr key value) = cs key $= cs value

lookupAttrs :: ST -> [Attr] -> Maybe ST
lookupAttrs _ [] = Nothing
lookupAttrs wantedKey (Attr key value : _) | key == wantedKey = Just value
lookupAttrs wantedKey (_ : as) = lookupAttrs wantedKey as

foreign import javascript unsafe
  "$1 === $2"
  (===) :: JSVal -> JSVal -> Bool

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


deriving instance FromJSVal (NoJSONRep JSVal)
deriving instance ToJSVal (NoJSONRep JSVal)
