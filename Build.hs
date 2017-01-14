import Development.Shake

import Data.Monoid


-- * project dirs

refineBackend, refineCommon, refineFrontend :: FilePath

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

buildBackend, buildCommon, buildFrontend, buildPrelude, hlint, setup :: String

buildBackend  = "build-backend"
buildCommon   = "build-common"
buildFrontend = "build-frontend"
buildPrelude  = "build-prelude"
hlint         = "hlint"
setup         = "setup"

-- * main

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles=".build", shakeVerbosity=Loud} $ do

  want
    [ buildBackend
    , buildCommon
    , buildFrontend
    , buildPrelude
    , hlint
    ]

  phony buildBackend $ do
    stackBuild refineBackend

  phony buildCommon $ do
    stackBuild refineCommon

  phony buildFrontend $ do
    stackBuild refineFrontend

  phony buildPrelude $ do
    stackBuild refinePrelude

  phony setup $ do
    let resolver = "nightly-2017-01-10"
    command_ [] "stack" ["install", "hlint", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "hlint", "--version"]
    command_ [] "stack" ["install", "hspec-discover", "--resolver", resolver]

  phony "clean" $ do
    command_ [Cwd refineBackend]  "rm" ["-rf", ".stack-work"]
    command_ [Cwd refineCommon]   "rm" ["-rf", ".stack-work"]
    command_ [Cwd refineFrontend] "rm" ["-rf", ".stack-work"]
    command_ [Cwd refinePrelude]  "rm" ["-rf", ".stack-work"]

  phony hlint $ do
    hlintProject refineBackend
    hlintProject refineCommon
    hlintProject refineFrontend
