{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.WebSocket where
#include "import_frontend.hs"

import           Control.Concurrent
import           System.IO.Unsafe
#ifdef __GHCJS__
import           JavaScript.Web.WebSocket
import           JavaScript.Web.MessageEvent
import           JavaScript.Web.CloseEvent
#endif

import Refine.Common.Rest
import Refine.Common.Types
import Refine.Frontend.Access
import Refine.Frontend.Document.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Route
import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Console


{-# NOINLINE webSocketMVar #-}
webSocketMVar :: MVar (WSSessionId, WebSocket)
webSocketMVar = unsafePerformIO newEmptyMVar

sendMessage :: WebSocket -> ToServer -> IO ()
sendMessage conn msg = send (cs $ encode msg) conn

initWebSocket :: (StoreData GlobalState, StoreAction GlobalState ~ GlobalAction, Dispatchable GlobalAction) => IO ()
initWebSocket = do
    let wsClose :: CloseEvent -> IO ()
        wsClose _ = do
          consoleLogJSStringM "WS" "connection closed"
          (n, _) <- takeMVar webSocketMVar
          openConnection $ Just n

        wsMessage :: MessageEvent -> IO ()
        wsMessage msg = case getData msg of
            StringData x -> maybe (error "websockets json decoding error") wsParsedMessage . decode $ cs x
            (BlobData _)        -> error "websockets error: BlobData not supported."
            (ArrayBufferData _) -> error "websockets error: ArrayBufferData not supported."

        wsParsedMessage :: ToClient -> IO ()
        wsParsedMessage = \case
              TCServerCache sc ->
                executeAction . action @GlobalState . CacheAction $ RefreshServerCache sc
              TCInvalidateKeys keys ->
                executeAction . action @GlobalState . CacheAction . InvalidateCacheItems
                $ Set.fromList keys
              TCRestrictKeys keys ->
                executeAction . action @GlobalState . CacheAction . RestrictCacheItems
                $ Set.fromList keys
              s@(TCGreeting n) -> do
                  (_, ws) <- takeMVar webSocketMVar
                  putMVar webSocketMVar (n, ws)
                  consoleLogJSStringM "WS" $ cs (show s)
              TCError e -> do
                  (n, ws) <- readMVar webSocketMVar
                  consoleLogJSStringM "WS" . cs $ show (n, e)
                  handleApiError ws e

              TCCreatedVDoc vid ->
                dispatchAndExec $ LoadVDoc vid
              TCCreatedGroup _gid -> pure ()
              TCRebase ->
                dispatchAndExec $ DocumentAction UpdateDocumentStateView

              TCCreateUserResp (Left err) ->
                dispatchAndExec . MainMenuAction $ MainMenuActionRegistrationError err
              TCCreateUserResp (Right _user) ->
                dispatchAndExec . CompositeAction $
                  [ MainMenuAction . MainMenuActionOpen $ MainMenuLogin MainMenuSubTabLogin
                  , MainMenuAction MainMenuActionClearErrors
                  ]
              TCLoginResp (Left err) ->
                dispatchAndExec . MainMenuAction . MainMenuActionLoginError . cs . show $ err
              TCLoginResp (Right user) -> do
                sendTS TSClearCache
                dispatchAndExec . SetCurrentUser . UserLoggedIn $ user ^. userID
                dispatchAndExec $ MainMenuAction MainMenuActionClose
                dispatchAndExec LoginGuardPop

              TCTranslations l10 ->
                dispatchAndExec $ ChangeTranslations l10

        handleApiError :: WebSocket -> ApiError -> IO ()
        handleApiError ws = \case
          ApiUnknownError                         -> ignore
          ApiVDocVersionError                     -> ignore
          ApiDBError (ApiDBNotFound _)            -> unloadvdoc >> resetroute  -- (e.g., because somebody hit an old route)
          ApiDBError _                            -> ignore
          ApiUserNotFound _                       -> ignore  -- handled via 'TCLoginResp'
          ApiUserNotLoggedIn                      -> ignore
          ApiUserCreationError _                  -> ignore  -- handled via 'TCCreateUserResp'
          ApiCsrfError _                          -> ignore  -- (doesn't even apply)
          ApiSessionInvalid                       -> reconnect
          ApiSanityCheckError _                   -> ignore
          ApiL10ParseErrors _                     -> ignore
          ApiUnauthorized _                       -> ignore
          ApiMergeError _                         -> ignore
          ApiRebaseError                          -> ignore
          ApiSmtpError                            -> ignore
          where
            ignore = pure ()
            reconnect = close Nothing Nothing ws  -- just need to close, re-open happens automatically.
            resetroute = changeRoute defaultRoute
            unloadvdoc = dispatchAndExec UnloadVDoc

        wsUrl :: JSString
        wsUrl = case clientCfg of
          ClientCfg port host ssl
            -> cs $ (if ssl then "wss://" else "ws://") <> host <> ":" <> cs (show port) <> "/"

        openConnection :: Maybe WSSessionId -> IO ()
        openConnection mid = handshake mid =<< do
            connect $ WebSocketRequest
                              wsUrl
                              []
                              (Just wsClose)
                              (Just wsMessage)

        handshake :: Maybe WSSessionId -> WebSocket -> IO ()
        handshake mid ws = do
            sendMessage ws $ TSGreeting mid
            putMVar webSocketMVar (fromMaybe (error "unknown WSSessionId") mid, ws)
            consoleLogJSStringM "WS" "connection opened"

    openConnection Nothing

sendTS :: ToServer -> IO ()
sendTS cmd = (`sendMessage` cmd) . snd =<< readMVar webSocketMVar

instance Sendable ToServer where
  sendToServer = sendTS


#if __GHCJS__
clientCfg :: ClientCfg
clientCfg = fromMaybe (error "clientCfg: decoding error!") . decode . cs $ js_clientCfg

#else
-- copied from ghcjs-base (JavaScript.Web.WebSocket, JavaScript.Web.MessageEvent), and mutated to
-- build with ghc.

data MessageEventData = StringData      JSString
                      | BlobData        ()  -- Blob
                      | ArrayBufferData ()  -- ArrayBuffer

data WebSocket

data WebSocketRequest = WebSocketRequest
  { url       :: JSString
  , protocols :: [JSString]
  , onClose   :: Maybe (CloseEvent -> IO ()) -- ^ called when the connection closes (at most once)
  , onMessage :: Maybe (MessageEvent -> IO ()) -- ^ called for each message
  }

data CloseEvent
data MessageEvent

connect :: WebSocketRequest -> IO WebSocket
connect = error "ghcjs base not available"

send :: JSString -> WebSocket -> IO ()
send = error "ghcjs base not available"

getData :: MessageEvent -> MessageEventData
getData = error "ghcjs base not available"

close :: Maybe Int -> Maybe JSString -> WebSocket -> IO ()
close = error "ghcjs base not available"

clientCfg :: ClientCfg
clientCfg = def

#endif

#ifdef __GHCJS__

foreign import javascript safe
  "$r = JSON.stringify(window.client_cfg);"
  js_clientCfg :: JSString

#else

{-# ANN js_clientCfg ("HLint: ignore Use camelCase" :: String) #-}
js_clientCfg :: JSString
js_clientCfg = error "javascript FFI not available in GHC"

#endif
