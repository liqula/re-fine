module Main where

import qualified React.Flux  as F
import React.Flux.Ajax (initAjax)
import qualified Refine.Frontend.RefineViews as RF

-- the first argument of reactRender is the id of the DOM element in index.html that the app will be rendered into
main :: IO ()
main = do
    initAjax
    F.reactRender "refine" RF.refineApp ()
