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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | FUTUREWORK: this module pulls its development mode status from the webpack config, not from
-- "Refine.Backend.Config".  we should unify that at some point.
module Refine.Frontend.Test.Console
  ( consoleLogJSString, consoleLogJSStringM
  , consoleLogJSVal, consoleLogJSValM
  , consoleLogJSON, consoleLogJSONM
  , consoleLogJSONAsString, consoleLogJSONAsStringM
  , gracefulError
  , weAreInDevMode
  )
where

import Data.Aeson (ToJSON, encode)
import Data.JSString ()  -- instance IsString JSString
import Data.String.Conversions (ConvertibleStrings, cs, (<>))
import GHCJS.Types (JSVal, JSString)

import Refine.Frontend.CS ()


consoleLogJSStringM :: (Monad m) => JSString -> JSString -> m ()
consoleLogJSStringM msg val = consoleLogJSString msg val `seq` pure ()

consoleLogJSValM :: (Monad m) => JSString -> JSVal -> m ()
consoleLogJSValM msg val = consoleLogJSVal msg val `seq` pure ()

consoleLogJSONM :: (Monad m, ToJSON a) => JSString -> a -> m ()
consoleLogJSONM msg val = consoleLogJSON msg val `seq` pure ()

consoleLogJSONAsStringM :: (Monad m, ConvertibleStrings s JSString) => JSString -> s -> m ()
consoleLogJSONAsStringM msg val = consoleLogJSONAsString msg val `seq` pure ()


consoleLogJSString :: JSString -> JSString -> ()
consoleLogJSString = if js_devMode then js_consoleLogJSString else \_ _ -> ()

foreign import javascript unsafe
  "console.log($1, $2)"
  js_consoleLogJSString :: JSString -> JSString -> ()


consoleLogJSVal :: JSString -> JSVal -> ()
consoleLogJSVal = if js_devMode then js_consoleLogJSVal else \_ _ -> ()

foreign import javascript unsafe
  "console.log($1, $2)"
  js_consoleLogJSVal :: JSString -> JSVal -> ()


-- | Write a 'ToJSON' instance to stdout (node) or the console (browser).  If you have a choice, use
-- 'js_consoleLogJSON' which is more efficient.  (No idea if there are char encoding issues here.  But
-- it's probably safe to use it for development.)
consoleLogJSON :: ToJSON a => JSString -> a -> ()
consoleLogJSON = if js_devMode then \str -> js_consoleLogJSON str . cs . encode else \_ _ -> ()

consoleLogJSONAsString :: ConvertibleStrings s JSString => JSString -> s -> ()
consoleLogJSONAsString = if js_devMode then \str -> js_consoleLogJSON str . cs else \_ _ -> ()

foreign import javascript unsafe
  "console.log($1, JSON.parse($2))"
  js_consoleLogJSON :: JSString -> JSString -> ()


weAreInDevMode :: Bool
weAreInDevMode = js_devMode

foreign import javascript unsafe
  "process.env.NODE_ENV === 'development' ? true : false"
  js_devMode :: Bool


-- | Log a recoverable error and stay in business.
--
-- FUTUREWORK: instead of logging to the console, this would be a good think to hear about on the
-- server side as well.  we could have an end-point that just receives frontend errors and logs.
-- This probably needs log levels to be useful and sufficiently non-invasive.
{-# ANN gracefulError ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
gracefulError :: ConvertibleStrings s JSString => s -> a -> a
gracefulError msg = consoleLogJSString ("\n\n\n***** " <> cs msg <> "\n\n\n") `seq` id
