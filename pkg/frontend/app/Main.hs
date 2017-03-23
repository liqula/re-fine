{-# LANGUAGE OverloadedStrings #-}

module Main where

import React.Flux.Ajax (initAjax)
import React.Flux (registerInitialStore, reactRenderView)

import Refine.Frontend.Types (emptyGlobalState)
import Refine.Frontend.Views (refineApp)

-- | the first argument of reactRender is the id of the DOM element in index.html that the app will be rendered into
main :: IO ()
main = do
    registerInitialStore emptyGlobalState
    initAjax
    reactRenderView "refine" refineApp
