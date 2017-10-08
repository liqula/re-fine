{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.WebSocket where
#include "import_frontend.hs"

import           Control.Concurrent
import           System.IO.Unsafe
import           System.Random
#ifdef __GHCJS__
import           JavaScript.Web.WebSocket
import           JavaScript.Web.MessageEvent
import           JavaScript.Web.CloseEvent
#endif

import Refine.Common.Error
import Refine.Common.Types
import Refine.Frontend.Access
import Refine.Frontend.Document.Types
import Refine.Frontend.Login.Types
import {-# SOURCE #-} Refine.Frontend.Login.Status
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Route
import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Console


{-# NOINLINE webSocketMVar #-}
webSocketMVar :: MVar (WSState, WebSocket)
webSocketMVar = unsafePerformIO newEmptyMVar

data WSState = WSSHandshake | WSSSession WSSessionId
  deriving (Eq, Show)

instance Sendable ToServer where
  sendToServer = sendTS

sendTS :: ToServer -> IO ()
sendTS cmd = (`sendMessage` cmd) . snd =<< readMVar webSocketMVar

sendMessage :: ToJSON toServer => WebSocket -> toServer -> IO ()
sendMessage conn msg = send (cs $ encode msg) conn

initWebSocket :: (StoreData GlobalState, StoreAction GlobalState ~ GlobalAction, Dispatchable GlobalAction) => IO ()
initWebSocket = openConnection Nothing
  where
   wsUrl :: JSString
   wsUrl = case clientCfg of
     ClientCfg port host ssl
       -> cs $ (if ssl then "wss://" else "ws://") <> host <> ":" <> cs (show port) <> "/"

   wsReq :: WebSocketRequest
   wsReq = WebSocketRequest wsUrl [] (Just wsClose) (Just wsMessage)

   wsClose :: CloseEvent -> IO ()
   wsClose _ = tryTakeMVar webSocketMVar >>= \case
     Nothing -> do  -- could happen if connection dies before handshakeSend is called.
       threadDelay =<< randomRIO (100 * 1000, 1000 * 1000)
       openConnection Nothing
     Just (st, _) -> openConnection $ case st of
       WSSHandshake -> Nothing
       WSSSession sid -> Just sid

   openConnection :: Maybe WSSessionId -> IO ()
   openConnection mid = connect wsReq >>= handshakeSend mid

   handshakeSend :: Maybe WSSessionId -> WebSocket -> IO ()
   handshakeSend mid conn = do
     putMVar webSocketMVar (WSSHandshake, conn)
     sendMessage conn $ maybe HandshakeToServerSyn HandshakeToServerSynWith mid

   wsMessage :: MessageEvent -> IO ()
   wsMessage msgev = readMVar webSocketMVar >>= \case
     (st, _) -> case (st, getData msgev) of
       (WSSHandshake,    StringData (decode . cs -> Just msg)) -> wsRunHandshake msg
       (WSSSession _sid, StringData (decode . cs -> Just msg)) -> wsRunToClient msg

       (_, StringData bad)    -> throwIO $ ErrorCall ("websockets error: bad package for this state: " <> show (st, bad))
       (_, BlobData _)        -> throwIO $ ErrorCall "websockets error: BlobData not supported."
       (_, ArrayBufferData _) -> throwIO $ ErrorCall "websockets error: ArrayBufferData not supported."

   wsRunHandshake :: HandshakeToClient -> IO ()
   wsRunHandshake = \case
     HandshakeToClientAcceptNew sid -> do
       conclude sid
       executeAction . action @GlobalState . CacheAction $ RestrictCacheItems mempty
       executeAction $ action @AccessState Logout
       -- TODO: anything else?
     HandshakeToClientAccept sid toClientQ -> do
       conclude sid
       wsRunToClient `mapM_` toClientQ
     where
       conclude :: WSSessionId -> IO ()
       conclude sid = do
         consoleLogJSStringM "WS" "connection opened"
         modifyMVar_ webSocketMVar $ \(_, conn) -> pure (WSSSession sid, conn)

   wsRunToClient :: ToClient -> IO ()
   wsRunToClient = \case
     TCServerCache sc ->
       executeAction . action @GlobalState . CacheAction $ RefreshServerCache sc
     TCInvalidateKeys keys ->
       executeAction . action @GlobalState . CacheAction . InvalidateCacheItems $ Set.fromList keys
     TCRestrictKeys keys ->
       executeAction . action @GlobalState . CacheAction . RestrictCacheItems $ Set.fromList keys
     TCError e -> do
         (st, conn) <- readMVar webSocketMVar
         consoleLogJSStringM "WS" . cs $ show (st, e)
         handleApiError conn e

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
       dispatchAndExec `mapM_` onLoginClick (UserLoggedIn (Right user))
       dispatchAndExec LoginGuardPop

     TCTranslations l10 ->
       dispatchAndExec $ ChangeTranslations l10

   handleApiError :: WebSocket -> ApiError -> IO ()
   handleApiError conn = \case
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
     ApiTimeoutError _                       -> ignore
     where
       ignore = pure ()
       reconnect = close Nothing (Just "disconnect after error") conn  -- this triggers auto-re-open.
       resetroute = changeRoute defaultRoute
       unloadvdoc = dispatchAndExec UnloadVDoc


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
