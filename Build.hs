{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad
import Data.Monoid
import Development.Shake


-- * project dirs

refineBackend, refineCommon, refineFrontend, refinePrelude :: FilePath

refineBackend  = "refine-backend"
refineCommon   = "refine-common"
refineFrontend = "refine-frontend"
refinePrelude  = "refine-prelude"


-- * actions

stackBuild :: FilePath -> Action ()
stackBuild project = do
  command_ [Cwd project] "stack" ["test", "--fast"]

hlintProject :: FilePath -> Action ()
hlintProject project = do
  command_ [] "stack"
    [ "exec", "--", "hlint"
    , "--hint=./refine-prelude/HLint.hs"
    , "./" <> project <> "/src"
    , "./" <> project <> "/test"
    ]


-- * commands

setup, buildPrelude, buildCommon, buildBackend, buildFrontend, buildAll, clean, distClean, hlint :: String

setup         = "setup"
buildPrelude  = "build-prelude"
buildCommon   = "build-common"
buildBackend  = "build-backend"
buildFrontend = "build-frontend"
buildAll      = "build-all"
clean         = "clean"
distClean     = "dist-clean"
hlint         = "hlint"


-- * main

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".build", shakeVerbosity = Loud } $ do

  want
    [ buildAll
    , hlint
    ]

  phony setup $ do
    let resolver = "nightly-2017-01-10"  -- (we need at least hlint v1.9.39, which is not in lts-7.15.)
    command_ [] "stack" ["install", "hlint", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "hlint", "--version"]
    command_ [] "stack" ["install", "hspec-discover", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "which", "hspec-discover"]

  phony buildPrelude $ do
    stackBuild refinePrelude

  phony buildCommon $ do
    stackBuild refineCommon

  phony buildBackend $ do
    stackBuild refineBackend

  phony buildFrontend $ do
    stackBuild refineFrontend

  phony buildAll $ do
    -- for building everything, we only need to go to backend and frontend.  prelude and common are
    -- compiled in both (two different compilers), and tested (as non-extra deps in stack.yaml) in
    -- backend.
    --
    -- (FUTUREWORK: there is no need to force sequential firing of these two targets.  we just need
    -- to figure out how to express that in shake.)
    stackBuild refineBackend
    stackBuild refineFrontend

  phony clean $ do
    forM_ [refinePrelude, refineCommon, refineBackend, refineFrontend] $ \pkg -> do
        command_ [Cwd pkg] "stack" ["clean"]

  phony distClean $ do
    forM_ [refinePrelude, refineCommon, refineBackend, refineFrontend] $ \pkg -> do
        command_ [Cwd pkg] "rm" ["-rf", ".stack-work"]

  phony hlint $ do
    hlintProject refinePrelude
    hlintProject refineCommon
    hlintProject refineBackend
    hlintProject refineFrontend
