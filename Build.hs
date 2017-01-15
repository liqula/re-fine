{-# OPTIONS_GHC -Wall -Werror #-}

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
  command_ [Cwd project] "stack" ["test", "--fast", project]

hlintProject :: FilePath -> Action ()
hlintProject project = do
  command_ [] "stack"
    [ "exec", "--", "hlint"
    , "--hint=./refine-prelude/HLint.hs"
    , "./" <> project <> "/src"
    , "./" <> project <> "/test"
    ]


-- * commands

buildBackend, buildCommon, buildFrontend, buildPrelude, hlint, setup, clean :: String

buildBackend  = "build-backend"
buildCommon   = "build-common"
buildFrontend = "build-frontend"
buildPrelude  = "build-prelude"
hlint         = "hlint"
setup         = "setup"
clean         = "clean"


-- * main

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".build", shakeVerbosity = Loud } $ do

  want
    [ buildPrelude
    , buildCommon
    , buildBackend
    , buildFrontend
    , hlint
    ]

  phony buildPrelude $ do
    stackBuild refinePrelude

  phony buildBackend $ do
    stackBuild refineBackend

  phony buildCommon $ do
    stackBuild refineCommon

  phony buildFrontend $ do
    stackBuild refineFrontend

  phony setup $ do  -- TODO: instead of phony, make this a pre-requisite of the all other targets.
    let resolver = "nightly-2017-01-10"
    command_ [] "stack" ["install", "hlint", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "hlint", "--version"]
    command_ [] "stack" ["install", "hspec-discover", "--resolver", resolver]

  phony clean $ do
    command_ [Cwd refinePrelude]  "rm" ["-rf", ".stack-work"]
    command_ [Cwd refineCommon]   "rm" ["-rf", ".stack-work"]
    command_ [Cwd refineBackend]  "rm" ["-rf", ".stack-work"]
    command_ [Cwd refineFrontend] "rm" ["-rf", ".stack-work"]

  phony hlint $ do
    hlintProject refinePrelude
    hlintProject refineCommon
    hlintProject refineBackend
    hlintProject refineFrontend
