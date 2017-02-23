module Refine.Frontend.ErrorHandling where

import Debug.Trace

{-# ANN gracefulError ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
gracefulError :: String -> a -> a
gracefulError = trace
