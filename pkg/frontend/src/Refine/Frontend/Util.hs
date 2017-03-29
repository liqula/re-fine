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

module Refine.Frontend.Util
where

import           Data.List (nub)
import qualified Data.JSString as JSS
import           Data.String.Conversions
import qualified Data.Text as ST
import           GHCJS.Types (JSVal)
import           React.Flux
import           Text.HTML.Parser (Attr(Attr))

import           Refine.Frontend.CS ()


-- | See also:
-- https://bitbucket.org/wuzzeb/react-flux/pull-requests/11/clarify-diversify-classnames-helper/diff
classNamesAny :: [(ST, Bool)] -> PropertyOrHandler handler
classNamesAny xs = "className" @= ST.unwords names
  where
    names = nub $ fst <$> filter snd xs

toClasses :: (ConvertibleStrings s JSS.JSString, ConvertibleStrings JSS.JSString s) => [s] -> s
toClasses = cs . JSS.unwords . filter (not . JSS.null) . fmap cs

toProperty :: Attr -> forall handler. PropertyOrHandler handler
toProperty (Attr key value) = cs key $= cs value

attribValueOf :: ST -> [Attr] -> Maybe ST
attribValueOf _ [] = Nothing
attribValueOf wantedKey (Attr key value : _) | key == wantedKey = Just value
attribValueOf wantedKey (_ : as) = attribValueOf wantedKey as

foreign import javascript unsafe
  "$1 === $2"
  js_eq :: JSVal -> JSVal -> Bool
