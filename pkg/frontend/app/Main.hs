{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Refine.Frontend.Prelude

import           React.Flux.Ajax (initAjax)
import           React.Flux (registerInitialStore, reactRenderView)

import Refine.Frontend.Store.Types
import Refine.Frontend.Access (emptyAccessState)
import Refine.Frontend.Document.Types (emptyEditorStore)
import Refine.Frontend.Views (refineApp)
import Refine.Frontend.WebSocket
import Refine.Frontend.Store (initRouting)

main :: IO ()
main = do
    registerInitialStore emptyGlobalState
    registerInitialStore emptyAccessState
    registerInitialStore emptyEditorStore
    initAjax
    initWebSocket
    initRouting
    reactRenderView "refine" refineApp
