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

stackBuild :: FilePath -> Action ()
stackBuild package = do
  command_ [Cwd package] "stack" ["setup"]
  command_ [Cwd package] "stack" ["test", "--fast"]

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
    [ "build-all"
    , "hlint-all"
    ]

  phony "setup" $ do
    let resolver = "nightly-2017-01-10"  -- (we need at least hlint v1.9.39, which is not in lts-7.15.)
    command_ [] "stack" ["install", "hlint", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "hlint", "--version"]
    command_ [] "stack" ["install", "hspec-discover", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "which", "hspec-discover"]

  phony "build-prelude" $ do
    stackBuild pkgPrelude

  phony "build-common" $ do
    stackBuild pkgCommon

  phony "build-backend" $ do
    stackBuild pkgBackend

  phony "build-frontend" $ do
    need ["build-frontend-npm"]
    stackBuild pkgFrontend

  phony "build-frontend-npm" $ do
    command_ [Cwd pkgFrontend] "npm" ["install"]

  phony "build-all" $ do
    -- for building everything, we only need to go to backend and frontend.  prelude and common are
    -- compiled in both (two different compilers), and tested (as non-extra deps in stack.yaml) in
    -- backend.
    need ["build-backend", "build-frontend"]

  phony "clean" $ do
    forM_ [pkgPrelude, pkgCommon, pkgBackend, pkgFrontend] $ \pkg -> do
        command_ [Cwd pkg] "stack" ["clean"]

  phony "dist-clean" $ do
    forM_ [pkgPrelude, pkgCommon, pkgBackend, pkgFrontend] $ \pkg -> do
        command_ [Cwd pkg] "rm" ["-rf", ".stack-work"]

  phony "hlint-prelude" $ do
    hlintPackage pkgPrelude

  phony "hlint-common" $ do
    hlintPackage pkgCommon

  phony "hlint-backend" $ do
    hlintPackage pkgBackend

  phony "hlint-frontend" $ do
    hlintPackage pkgFrontend

  phony "hlint-all" $ do
    need ["hlint-prelude", "hlint-common", "hlint-backend", "hlint-frontend"]
