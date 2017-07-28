{-# LANGUAGE OverloadedStrings #-}

module Main where

import React.Flux.Ajax (initAjax)
import React.Flux (registerInitialStore, reactRenderView)

import Refine.Frontend.Store.Types
import Refine.Frontend.Views (refineApp)

main :: IO ()
main = do
    registerInitialStore emptyGlobalState
    initAjax
    reactRenderView "refine" refineApp
