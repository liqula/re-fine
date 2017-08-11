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

module Refine.Frontend.WebSocket where

import Refine.Frontend.Prelude

import           Control.Concurrent
import           System.IO.Unsafe
#ifdef __GHCJS__
import           JavaScript.Web.WebSocket
import           JavaScript.Web.MessageEvent
#endif

import Refine.Common.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Store()

{-# NOINLINE webSocketMVar #-}
webSocketMVar :: MVar ([CacheKey] -> IO ())
webSocketMVar = unsafePerformIO newEmptyMVar

initWebSocket :: IO ()
initWebSocket = do
    let wsClose _ = do
          putStrLn "websockets connection closed"
          _ <- takeMVar webSocketMVar
          openConnection

        wsMessage msg = do
          case getData msg of
            StringData x -> case decode $ cs x of
              Just (Just sc) -> executeAction . action @GlobalState $ RefreshServerCache sc
              Just Nothing -> putStrLn "ping"
              _ -> error "websockets json decoding error"
            _ -> error "websockets error"

        openConnection = do
            ws <- connect $ WebSocketRequest "ws://localhost:3000/"
                                                 []
                                                 (Just wsClose)
                                                 (Just wsMessage)
            putMVar webSocketMVar $ \keys -> send (cs $ encode keys) ws
            putStrLn "websocket connection opened"

    openConnection

#ifndef __GHCJS__
data MessageEventData = StringData JSString
                      | OtherMessageEventData

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
#endif
