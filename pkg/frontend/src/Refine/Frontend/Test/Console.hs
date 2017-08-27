{-# LANGUAGE CPP #-}
#include "language.hs"

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
#include "import_frontend.hs"


consoleLogJSStringM :: HasCallStack => (Monad m) => JSString -> JSString -> m ()
consoleLogJSStringM msg val = consoleLogJSString msg val `seq` pure ()

consoleLogJSValM :: HasCallStack => (Monad m) => JSString -> JSVal -> m ()
consoleLogJSValM msg val = consoleLogJSVal msg val `seq` pure ()

consoleLogJSONM :: HasCallStack => (Monad m, ToJSON a) => JSString -> a -> m ()
consoleLogJSONM msg val = consoleLogJSON msg val `seq` pure ()

consoleLogJSONAsStringM :: HasCallStack => (Monad m, ConvertibleStrings s JSString) => JSString -> s -> m ()
consoleLogJSONAsStringM msg val = consoleLogJSONAsString msg val `seq` pure ()


consoleLogJSString :: HasCallStack => JSString -> JSString -> ()
consoleLogJSString = if js_devMode then js_consoleLogJSString else \_ _ -> ()

consoleLogJSVal :: HasCallStack => JSString -> JSVal -> ()
consoleLogJSVal = if js_devMode then js_consoleLogJSVal else \_ _ -> ()

-- | Write a 'ToJSON' instance to stdout (node) or the console (browser).  If you have a choice, use
-- 'js_consoleLogJSON' which is more efficient.  (No idea if there are char encoding issues here.  But
-- it's probably safe to use it for development.)
consoleLogJSON :: HasCallStack => ToJSON a => JSString -> a -> ()
consoleLogJSON = if js_devMode then \str -> js_consoleLogJSON str . cs . encode else \_ _ -> ()

consoleLogJSONAsString :: HasCallStack => ConvertibleStrings s JSString => JSString -> s -> ()
consoleLogJSONAsString = if js_devMode then \str -> js_consoleLogJSON str . cs else \_ _ -> ()

weAreInDevMode :: HasCallStack => Bool
weAreInDevMode = js_devMode

-- | Log a recoverable error and stay in business.
--
-- FUTUREWORK: instead of logging to the console, this would be a good think to hear about on the
-- server side as well.  we could have an end-point that just receives frontend errors and logs.
-- This probably needs log levels to be useful and sufficiently non-invasive.
{-# ANN gracefulError ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
gracefulError :: HasCallStack => ConvertibleStrings s JSString => s -> a -> a
gracefulError msg = consoleLogJSString ("\n\n\n***** " <> cs msg <> "\n\n\n") `seq` id


windowAlert :: HasCallStack => (MonadIO m, ConvertibleStrings s JSString) => s -> m ()
windowAlert = liftIO . js_alert . cs

windowAlertST :: HasCallStack => MonadIO m => ST -> m ()
windowAlertST = windowAlert

#ifdef __GHCJS__

foreign import javascript safe
  "console.log($1, $2)"
  js_consoleLogJSString :: JSString -> JSString -> ()

foreign import javascript safe
  "console.log($1, $2)"
  js_consoleLogJSVal :: JSString -> JSVal -> ()

foreign import javascript safe
  "console.log($1, JSON.parse($2))"
  js_consoleLogJSON :: JSString -> JSString -> ()

foreign import javascript safe
  "process.env.NODE_ENV === 'development' ? true : false"
  js_devMode :: Bool

foreign import javascript safe
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
