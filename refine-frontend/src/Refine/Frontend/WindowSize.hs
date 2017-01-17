module Refine.Frontend.WindowSize where



import           Control.Concurrent (forkIO)
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String (fromString)
--import           Data.String.Conversions
import           React.Flux
import           React.Flux.Lifecycle
import           GHCJS.Foreign.Callback (Callback, asyncCallback)
import           GHCJS.Types (JSString)

import qualified Refine.Frontend.RefineStore as RS


newtype WindowSizeProps = WindowSizeProps
    { _currentSize :: RS.WindowSize
    }

windowSize :: ReactView WindowSizeProps
windowSize = defineLifecycleView "WindowSize" () lifecycleConfig
   { lRender = \_state (WindowSizeProps size) ->
         -- why oh why do I need a conversion here?
         -- TODO debug output; remove in production
         span_ [fromString "className" $= fromString "layout-indicator"] . elemString $ "layout: " <> show size
   , lComponentDidMount = Just $ \_ _ _ -> do
           consoleLog $ fromString "Component did mount"
           cb <- asyncCallback setWindowSize
           js_windowAddEventListener (fromString "resize") cb
           setWindowSize
   , lComponentWillUnmount = Just $ \_ _ -> do
           cb <- asyncCallback setWindowSize
           js_windowRemoveEventListener (fromString "resize") cb
   }

windowSize_ :: WindowSizeProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
windowSize_ = view windowSize

setWindowSize :: IO ()
setWindowSize = do
    width <- js_getWindowWidth
    _ <- forkIO $ do
       let actions = RS.dispatch . RS.SetWindowSize $ RS.toSize width
       forM_ actions executeAction
    return ()

foreign import javascript unsafe
-- the internet says we should check window.innerWidth and document.documentElement.clientWidth first,
-- and we should get the body via document.getElementsByTagName('body')[0]
-- but Tom does not do that either -- so?!
-- (In my Chrome, they are all available and contain the same values anyway...)
  "document.body.clientWidth"
  js_getWindowWidth :: IO Int


foreign import javascript unsafe
    "window.addEventListener($1, $2)"
    js_windowAddEventListener :: JSString -> Callback (IO ()) -> IO ()

foreign import javascript unsafe
    "window.removeEventListener($1, $2)"
    js_windowRemoveEventListener :: JSString -> Callback (IO ()) -> IO ()

foreign import javascript unsafe
  "console.log($1)"
  consoleLog :: JSString -> IO ()
