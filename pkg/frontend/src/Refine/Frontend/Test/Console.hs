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
  , windowAlert
  , windowAlertST
  )
where

import Refine.Frontend.Prelude


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

consoleLogJSVal :: JSString -> JSVal -> ()
consoleLogJSVal = if js_devMode then js_consoleLogJSVal else \_ _ -> ()

-- | Write a 'ToJSON' instance to stdout (node) or the console (browser).  If you have a choice, use
-- 'js_consoleLogJSON' which is more efficient.  (No idea if there are char encoding issues here.  But
-- it's probably safe to use it for development.)
consoleLogJSON :: ToJSON a => JSString -> a -> ()
consoleLogJSON = if js_devMode then \str -> js_consoleLogJSON str . cs . encode else \_ _ -> ()

consoleLogJSONAsString :: ConvertibleStrings s JSString => JSString -> s -> ()
consoleLogJSONAsString = if js_devMode then \str -> js_consoleLogJSON str . cs else \_ _ -> ()

weAreInDevMode :: Bool
weAreInDevMode = js_devMode

-- | Log a recoverable error and stay in business.
--
-- FUTUREWORK: instead of logging to the console, this would be a good think to hear about on the
-- server side as well.  we could have an end-point that just receives frontend errors and logs.
-- This probably needs log levels to be useful and sufficiently non-invasive.
{-# ANN gracefulError ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
gracefulError :: ConvertibleStrings s JSString => s -> a -> a
gracefulError msg = consoleLogJSString ("\n\n\n***** " <> cs msg <> "\n\n\n") `seq` id


windowAlert :: (MonadIO m, ConvertibleStrings s JSString) => s -> m ()
windowAlert = liftIO . js_alert . cs

windowAlertST :: MonadIO m => ST -> m ()
windowAlertST = windowAlert

#ifdef __GHCJS__

foreign import javascript unsafe
  "console.log($1, $2)"
  js_consoleLogJSString :: JSString -> JSString -> ()

foreign import javascript unsafe
  "console.log($1, $2)"
  js_consoleLogJSVal :: JSString -> JSVal -> ()

foreign import javascript unsafe
  "console.log($1, JSON.parse($2))"
  js_consoleLogJSON :: JSString -> JSString -> ()

foreign import javascript unsafe
  "process.env.NODE_ENV === 'development' ? true : false"
  js_devMode :: Bool

foreign import javascript unsafe
  "window.alert($1)"
  js_alert :: JSString -> IO ()

#else

{-# ANN js_consoleLogJSString ("HLint: ignore Use camelCase" :: String) #-}
js_consoleLogJSString :: JSString -> JSString -> ()
js_consoleLogJSString = error "javascript FFI not available in GHC"

{-# ANN js_consoleLogJSVal ("HLint: ignore Use camelCase" :: String) #-}
js_consoleLogJSVal :: JSString -> JSVal -> ()
js_consoleLogJSVal = error "javascript FFI not available in GHC"

{-# ANN js_consoleLogJSON ("HLint: ignore Use camelCase" :: String) #-}
js_consoleLogJSON :: JSString -> JSString -> ()
js_consoleLogJSON = error "javascript FFI not available in GHC"

{-# ANN js_devMode ("HLint: ignore Use camelCase" :: String) #-}
js_devMode :: Bool
js_devMode = error "javascript FFI not available in GHC"

{-# ANN js_alert ("HLint: ignore Use camelCase" :: String) #-}
js_alert :: JSString -> IO ()
js_alert = error "javascript FFI not available in GHC"

#endif
