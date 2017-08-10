{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Refine.Frontend.Prelude

import           React.Flux.Ajax (initAjax)
import           React.Flux (registerInitialStore, reactRenderView)
import qualified JavaScript.Web.WebSocket as WS
import           JavaScript.Web.MessageEvent
import           Control.Concurrent

import Refine.Frontend.Store.Types
import Refine.Frontend.Views (refineApp)

main :: IO ()
main = do
    registerInitialStore emptyGlobalState
    initAjax
    let wsClose _ = putStrLn "websockets connection closed"
        wsMessage msg = do
          putStrLn "websocket message: "
          case getData msg of
            StringData jsstring -> putStrLn $ "received from server: " <> show jsstring
            _ -> pure ()
    ws <- WS.connect $ WS.WebSocketRequest "ws://localhost:3000/"
                                         []
                                         (Just wsClose)
                                         (Just wsMessage)
    let sendloop i = do
          WS.send (cs $ show i) ws
          threadDelay 3540000
          sendloop (i + 1)
    _ <- forkIO $ sendloop (0 :: Int)

    reactRenderView "refine" refineApp
