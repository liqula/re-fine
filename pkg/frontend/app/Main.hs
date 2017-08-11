{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Refine.Frontend.Prelude

import           React.Flux.Ajax (initAjax)
import           React.Flux (registerInitialStore, reactRenderView)

import Refine.Frontend.Store.Types
import Refine.Frontend.Views (refineApp)
import Refine.Frontend.WebSocket

main :: IO ()
main = do
    registerInitialStore emptyGlobalState
    initAjax
    initWebSocket
    reactRenderView "refine" refineApp
