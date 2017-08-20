{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.WebSocket where

import Refine.Frontend.Prelude

import qualified Data.Set as Set
import           Control.Concurrent
import           System.IO.Unsafe
#ifdef __GHCJS__
import           JavaScript.Web.WebSocket
import           JavaScript.Web.MessageEvent
#endif

import Refine.Common.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Access


{-# NOINLINE webSocketMVar #-}
webSocketMVar :: MVar (WSSessionId, WebSocket)
webSocketMVar = unsafePerformIO newEmptyMVar

sendMessage :: WebSocket -> ToServer -> IO ()
sendMessage conn msg = send (cs $ encode msg) conn

initWebSocket :: (StoreData GlobalState, StoreAction GlobalState ~ GlobalAction, Dispatchable GlobalAction) => IO ()
initWebSocket = do
    let wsClose _ = do
          putStrLn "websockets connection closed"
          (n, _) <- takeMVar webSocketMVar
          openConnection $ Just n

        wsMessage msg = do
          case getData msg of
            StringData x -> case fromMaybe (error "websockets json decoding error") . decode $ cs x of
              TCServerCache sc ->
                executeAction . action @GlobalState . CacheAction $ RefreshServerCache sc
              TCInvalidateKeys keys ->
                executeAction . action @GlobalState . CacheAction . InvalidateCacheItems
                $ Set.fromList keys
              TCRestrictKeys keys ->
                executeAction . action @GlobalState . CacheAction . RestrictCacheItems
                $ Set.fromList keys
              TCPing -> putStrLn "ping"
              TCGreeting n -> do
                  (_, putfun) <- takeMVar webSocketMVar
                  putMVar webSocketMVar (n, putfun)
                  putStrLn $ "WSSessionId is " <> show n
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
                dispatchAndExec . SetCurrentUser $ UserLoggedIn user
                dispatchAndExec $ MainMenuAction MainMenuActionClose
                dispatchAndExec LoginGuardPop

              TCTranslations l10 ->
                dispatchAndExec $ ChangeTranslations l10

            (BlobData _)        -> error "websockets error: BlobData not supported."
            (ArrayBufferData _) -> error "websockets error: ArrayBufferData not supported."

        wsUrl = case clientCfg of
          ClientCfg port host ssl
            -> cs $ (if ssl then "wss://" else "ws://") <> host <> ":" <> cs (show port) <> "/"


        openConnection mid = do
            ws <- connect $ WebSocketRequest
                              wsUrl
                              []
                              (Just wsClose)
                              (Just wsMessage)

            sendMessage ws $ TSGreeting mid
            putMVar webSocketMVar (fromMaybe (error "unknown WSSessionId") mid, ws)
            putStrLn "websocket connection opened"

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
