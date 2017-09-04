{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Route where
#include "import_frontend.hs"

import           GHCJS.Foreign.Callback (Callback, asyncCallback1)


data Route
  = Help
  | Login
  | Register
  | Groups
  | GroupProcesses (ID Common.Group)
  | GroupMembers (ID Common.Group)
  | Process (ID Common.VDoc)
  deriving (Eq, Show, Generic)

rrender :: Route -> JSString
rrender = \case
  Help                           -> "#help"
  Login                          -> "#login"
  Register                       -> "#register"
  Groups                         -> "#groups"
  GroupProcesses (Common.ID gid) -> "#group-" <> cs (show gid) <> "-procs"
  GroupMembers (Common.ID gid)   -> "#group-" <> cs (show gid) <> "-members"
  Process (Common.ID vid)        -> "#vdoc-" <> cs (show vid)

newtype RouteParseError = RouteParseError String

rparse :: JSString -> Either RouteParseError Route
rparse hash = case drop 1 (cs hash) of
  "help"                             -> pure $ Help
  "login"                            -> pure $ Login
  "register"                         -> pure $ Register
  "groups"                           -> pure $ Groups
  (strip "process" -> Just i)        -> pure . Process $ Common.ID i
  (strip "groupProcesses" -> Just i) -> pure . GroupProcesses $ Common.ID i
  (strip "groupMembers" -> Just i)   -> pure . GroupMembers $ Common.ID i
  bad                                -> throwError . RouteParseError $ "could not parse route: " <> show bad
  where
    strip s k | take (length s) k == s = case reads $ drop (length s) k of
                  [(a, "")] -> Just a
                  _ -> Nothing
              | otherwise = Nothing


onLocationHashChange :: (Either RouteParseError Route -> IO ()) -> IO ()
onLocationHashChange f = do
  cb <- asyncCallback1 (f . rparse . pFromJSVal)
  js_attachLocationHashCb cb

currentRoute :: MonadIO m => m (Either RouteParseError Route)
currentRoute = rparse <$> liftIO js_getLocationHash

changeRoute :: MonadIO m => Route -> m ()
changeRoute = liftIO . js_setLocationHash . rrender


-- * foreign

#ifdef __GHCJS__

foreign import javascript unsafe
  "window.onhashchange = function() {$1(location.hash.toString());};"
  js_attachLocationHashCb :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "window.location.hash"
  js_getLocationHash :: IO JSString

foreign import javascript safe
  "window.location.hash = $1;"
  js_setLocationHash :: JSString -> IO ()

#else

{-# ANN js_attachLocationHashCb ("HLint: ignore Use camelCase" :: String) #-}
js_attachLocationHashCb :: Callback (JSVal -> IO ()) -> IO ()
js_attachLocationHashCb = error "javascript FFI not available in GHC"

{-# ANN js_getLocationHash ("HLint: ignore Use camelCase" :: String) #-}
js_getLocationHash :: IO JSString
js_getLocationHash = error "javascript FFI not available in GHC"

{-# ANN js_setLocationHash ("HLint: ignore Use camelCase" :: String) #-}
js_setLocationHash :: JSString -> IO ()
js_setLocationHash = error "javascript FFI not available in GHC"

#endif
