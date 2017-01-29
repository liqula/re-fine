{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad
import Data.Monoid
import Development.Shake


refineOptions :: ShakeOptions
refineOptions = shakeOptions
  { shakeFiles = ".build"
  , shakeVerbosity = Loud
  , shakeThreads = 1  -- set to 0 to use as many threads as you have cores.
                      -- FIXME: parallel rule execution messes up stdout.  has anybody fixed that
                      -- for shake?  do it like in stack?
  }


-- * package dirs

pkgBackend, pkgCommon, pkgFrontend, pkgPrelude :: FilePath

pkgBackend  = "pkg/backend"
pkgCommon   = "pkg/common"
pkgFrontend = "pkg/frontend"
pkgPrelude  = "pkg/prelude"


-- * actions

stackTest :: FilePath -> Action ()
stackTest package = do
  command_ [Cwd package] "stack" ["setup"]
  command_ [Cwd package] "stack" ["test", "--fast"]

stackBuildFast :: FilePath -> Action ()
stackBuildFast package = do
  command_ [Cwd package] "stack" ["setup"]
  command_ [Cwd package] "stack" ["build", "--fast"]

hlintPackage :: FilePath -> Action ()
hlintPackage package = do
  command_ [] "stack"
    [ "exec", "--", "hlint"
    , "--hint=" <> pkgPrelude <> "/HLint.hs"
    , "./" <> package <> "/src"
    , "./" <> package <> "/test"
    ]


-- * main

main :: IO ()
main = shakeArgs refineOptions $ do
  want
    [ "test"
    , "hlint"
    ]

  phony "setup" $ do
    let resolver = "nightly-2017-01-10"  -- (we need at least hlint v1.9.39, which is not in lts-7.15.)
    command_ [] "stack" ["install", "hlint", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "hlint", "--version"]
    command_ [] "stack" ["install", "hspec-discover", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "which", "hspec-discover"]

  phony "test-prelude" $ do
    stackTest pkgPrelude

  phony "test-common" $ do
    stackTest pkgCommon

  phony "test-backend" $ do
    stackTest pkgBackend

  phony "test-frontend" $ do
    need ["build-frontend-npm"]
    stackTest pkgFrontend

  phony "test" $ do
    -- for building everything, we only need to go to backend and frontend.  prelude and common are
    -- compiled in both (two different compilers), and tested (as non-extra deps in stack.yaml) in
    -- backend.
    need ["test-backend", "test-frontend"]


  phony "build-prelude" $ do
    stackBuildFast pkgPrelude

  phony "build-common" $ do
    stackBuildFast pkgCommon

  phony "build-backend" $ do
    stackBuildFast pkgBackend

  phony "build-frontend" $ do
    need ["build-frontend-npm"]
    stackBuildFast pkgFrontend

  phony "build-frontend-npm" $ do
    command_ [Cwd pkgFrontend] "npm" ["install"]

  phony "build" $ do
    -- for building everything, we only need to go to backend and frontend.  prelude and common are
    -- compiled in both (two different compilers), and tested (as non-extra deps in stack.yaml) in
    -- backend.
    need ["build-backend", "build-frontend"]


  phony "hlint-prelude" $ do
    hlintPackage pkgPrelude

  phony "hlint-common" $ do
    hlintPackage pkgCommon

  phony "hlint-backend" $ do
    hlintPackage pkgBackend

  phony "hlint-frontend" $ do
    hlintPackage pkgFrontend

  phony "hlint" $ do
    need ["hlint-prelude", "hlint-common", "hlint-backend", "hlint-frontend"]


  phony "clean" $ do
    forM_ [pkgPrelude, pkgCommon, pkgBackend, pkgFrontend] $ \pkg -> do
        command_ [Cwd pkg] "stack" ["clean"]

  phony "dist-clean" $ do
    forM_ [pkgPrelude, pkgCommon, pkgBackend, pkgFrontend] $ \pkg -> do
        command_ [Cwd pkg] "rm" ["-rf", ".stack-work"]
