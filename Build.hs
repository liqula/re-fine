{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad
import Data.Monoid
import Development.Shake
import System.Exit
import System.Process


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

stackBuildOptimal :: FilePath -> Action ()
stackBuildOptimal package = do
  command_ [Cwd package] "stack" ["setup"]
  command_ [Cwd package] "stack" ["build", "--split-objs", "--ghc-options", "-O2"]

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
  want ["test-all"]

  phony "test-all" $ do
    need ["setup"]
    need ["test", "hlint"]

  phony "setup" $ do
    let resolver = "lts-8.11"
    command_ [] "stack" ["install", "hlint", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "hlint", "--version"]
    command_ [] "stack" ["install", "hspec-discover", "--resolver", resolver]
    command_ [] "stack" ["exec", "--", "which", "hspec-discover"]
    command_ [] "stack" ["install", "happy", "--resolver", resolver]  -- (needed for pretty-show package)

    command_ [Cwd pkgPrelude]  "stack" ["setup"]
    command_ [Cwd pkgCommon]   "stack" ["setup"]
    command_ [Cwd pkgBackend]  "stack" ["setup"]
    command_ [Cwd pkgFrontend] "stack" ["setup"]

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
    need ["build-frontend-npm", "build-frontend-trans"]
    stackBuildFast pkgFrontend
    command_ [Cwd pkgFrontend] "make" []

  phony "build-frontend-npm" $ do  -- if this fails, check #40.
    command_ [Cwd pkgFrontend] "node" ["--version"]
    command_ [Cwd pkgFrontend] "stack" ["exec", "--", "node", "--version"]
    command_ [Cwd pkgFrontend] "npm" ["install"]
    command_ [Cwd pkgFrontend] "npm" ["prune"]  -- remove unused dependencies

  phony "build-frontend-trans" $ do
    command_ [] "./scripts/i18n.hs"
      [ "pkg/frontend/src/"
      , "pkg/frontend/src/Refine/Frontend/TKey.hs"
      , "Refine.Frontend.TKey"
      , "po/"
      ]

  phony "build" $ do
    -- for building everything, we only need to go to backend and frontend.  prelude and common are
    -- compiled in both (two different compilers), and tested (as non-extra deps in stack.yaml) in
    -- backend.
    need ["build-backend", "build-frontend"]

  phony "build-optimal" $ do
    need ["clean"]
    stackBuildOptimal pkgBackend
    need ["build-frontend-npm", "build-frontend-trans"]
    stackBuildOptimal pkgFrontend
    command_ [Cwd pkgFrontend] "make" ["optimize"]

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


  phony "clean-prelude" $ do
    command_ [Cwd pkgPrelude] "stack" ["clean"]

  phony "clean-common" $ do
    command_ [Cwd pkgCommon] "stack" ["clean"]

  phony "clean-backend" $ do
    command_ [Cwd pkgBackend] "stack" ["clean"]

  phony "clean-frontend" $ do
    command_ [Cwd pkgFrontend] "stack" ["clean"]
    command_ [Cwd pkgFrontend] "rm" ["-rf", "js-build"]

  phony "clean" $ do
    need ["clean-prelude", "clean-common", "clean-backend", "clean-frontend"]

  phony "dist-clean" $ do
    need ["clean"]
    forM_ [pkgPrelude, pkgCommon, pkgBackend, pkgFrontend] $ \pkg -> do
        command_ [Cwd pkg] "rm" ["-rf", ".stack-work"]


  -- run frontend in development mode
  phony "run-dev" $ do
    let belog = pkgBackend <//> "dev.log"
        felog = pkgFrontend <//> "dev.log"
        serverconf = "server.conf"

    need ["build-backend", "build-frontend"]
    need [pkgBackend <//> serverconf]

    beh :: ProcessHandle <- cmd
      [Cwd pkgBackend, FileStdout belog, FileStderr belog] "stack" ["exec", "--", "refine", "server.conf"]

    feh :: ProcessHandle <- cmd
      [Cwd pkgFrontend, FileStdout felog, FileStderr felog] "npm" ["start"]

    liftIO $ do
      putStrLn `mapM_`
        [ ""
        , "development server is not running."
        , "interrupt if you are done or if you want shake back."
        , ""
        , "to see what's going on, try this:"
        , "tail -f " <> show belog
        , "tail -f " <> show felog
        , ""
        ]
      ExitSuccess <- waitForProcess beh
      ExitSuccess <- waitForProcess feh
      pure ()

  pkgBackend <//> "server.conf" %> \out -> do
    yes <- doesFileExist out
    unless yes . fail $
      "\n*** could not find backend config at " <> show out <> "." <>
      "\n*** try `cd pkg/backend && stack exec -- refine` and copy the output to server.config."
