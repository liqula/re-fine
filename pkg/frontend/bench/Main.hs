module Main where

import Data.Time.Clock

import Refine.Frontend.Util (js_getScrollOffset)

{-
-- bug: criterion-1.2.0.0 use some C foreign functions not defined with ghcjs

import Criterion.Main

main :: IO ()
main = defaultMain [
    bench "getScrollOffset" $ nfIO js_getScrollOffset
  ]
-}

-- foreign import javascript unsafe "(function() { return pageYOffset; })()"    1000000 call   1.56s
-- foreign import javascript unsafe "$r = pageYOffset"                          1000000 call   1.50s
-- foreign import javascript safe "$r = pageYOffset"                            1000000 call   1.53s
main :: IO ()
main = do
    t1 <- getCurrentTime
    mapM_ (\_ -> js_getScrollOffset) [1 :: Int ..1000000]
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
