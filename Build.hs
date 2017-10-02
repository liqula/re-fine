{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad
import Data.Monoid
import Development.Shake
import System.Exit
import System.FilePath
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

nixShell :: String -> Action ()
nixShell act = command_ [Cwd pkgFrontend, Shell] "nix-shell"
  ["--attr", "env", "build.nix", "--run", show act]

stackTest :: FilePath -> Action ()
stackTest package = do
  command_ [Cwd package, Shell] "stack" ["test", "--fast", "--test-arguments=$TEST_ARGS"]

stackBuildFast :: FilePath -> Action ()
stackBuildFast package = do
  command_ [Cwd package] "stack" ["build", "--fast"]

stackBuildOptimal :: FilePath -> Action ()
stackBuildOptimal package = do
  command_ [Cwd package] "stack" ["build", "--split-objs", "--ghc-options", "-O2"]

hlintPath :: [String] -> FilePath -> Action ()
hlintPath extra path = do
  command_ [] "stack" $
    [ "exec", "--", "hlint"
    , "--hint=" <> pkgPrelude <> "/HLint.hs"
    ] <> extra <>
    [ "./" <> path
    ]

hlintPackage :: FilePath -> Action ()
hlintPackage package = hlintPath ["--cpp-include=" <> package <> "/src"] `mapM_` ((package <>) <$> ["/src", "/test"])


-- * main

main :: IO ()
main = shakeArgs refineOptions $ do
  want ["test-all"]

  phony "test-all" $ do
    need ["setup"]
    need ["test"]
    need ["hlint-all"]

  phony "setup" $ do
    let resolver = "lts-8.11"
    -- command_ [] "stack" [resolver, "setup"]  -- (this is done in ./build, before we can fire up the shake code.)
    command_ [] "stack" ["--resolver", resolver, "install", "hlint"]
    command_ [] "stack" ["--resolver", resolver, "exec", "--", "hlint", "--version"]
    command_ [] "stack" ["--resolver", resolver, "install", "hspec-discover"]
    command_ [] "stack" ["--resolver", resolver, "exec", "--", "which", "hspec-discover"]
    command_ [] "stack" ["--resolver", resolver, "install", "happy"]  -- (needed for pretty-show package)
    command_ [] "stack" ["--resolver", resolver, "install", "hpack"]

    command_ [Cwd pkgPrelude]  "stack" ["setup"]
    command_ [Cwd pkgCommon]   "stack" ["setup"]
    command_ [Cwd pkgBackend]  "stack" ["setup"]

    command_ [] "cabal" ["update"]

  phony "test-prelude" $ do
    stackTest pkgPrelude

  phony "test-common" $ do
    stackTest pkgCommon

  phony "test-backend" $ do
    stackTest pkgBackend

  phony "test-frontend-warmup" $ do
    command_ [Cwd pkgFrontend] "hspec-discover" ["test/", "", "test/Spec.hs", "--module-name=Spec"]
    -- usually, you just have one line in test/Spec.hs that does this implicitly:
    --
    -- >>> {-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}
    --
    -- however, this has proven not very reliable in combination with stack, nix, ghcjs, and all the
    -- other build tools we're using.  see #278 for more details.
    -- (note that the second argument is not used by hspec-discover.  i think it's just there to
    -- accomodate ghc if called via `-pgmF`.

  phony "test-frontend" $ do
    need ["build-frontend", "test-frontend-warmup"]
    nixShell "cabal configure --ghcjs && cabal test --test-options=\"$TEST_ARGS\""

  phony "accept" $ do
    need ["build-frontend", "build-backend"]
    command_ [Cwd "accept"] "stack" ["build", "--fast"]
    command_ [Cwd "accept"] "stack" ["exec", "--", "selenium", "install"]
    command_ [Cwd "accept"] "stack" ["exec", "--", "selenium", "stop"]
    command_ [Cwd "accept"] "stack" ["exec", "--", "selenium", "clean"]
    command_ [Cwd "accept"] "stack" ["exec", "--", "accept"]

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
    need ["build-frontend-prepare"]
    nixShell "cabal configure --ghcjs -f -ghc-O2 && cabal build"
    command_ [Cwd pkgFrontend] "make" []

  phony "build-frontend-optimize" $ do
    need ["build-frontend-prepare"]
    nixShell "cabal configure --ghcjs && cabal build"
    command_ [Cwd pkgFrontend] "make" ["optimize"]

  phony "build-frontend-prepare" $ do
    need ["build-frontend-hpack", "build-frontend-npm", "build-frontend-icons", "build-frontend-trans"]

  phony "build-frontend-hpack" $ do
    -- (needed because build.nix does not to that implicitly yet, like stack did.)
    command_ [Cwd pkgPrelude] "hpack" []
    command_ [Cwd pkgPrelude, FileStdout $ pkgPrelude </> "default.nix"] "cabal2nix" ["."]
    command_ [Cwd pkgCommon] "hpack" []
    command_ [Cwd pkgCommon, FileStdout $ pkgCommon </> "default.nix"] "cabal2nix" ["."]

  phony "build-frontend-npm" $ do  -- if this fails, check #40.
    command_ [Cwd pkgFrontend] "node" ["--version"]
    command_ [Cwd pkgFrontend] "stack" ["exec", "--", "node", "--version"]
    command_ [Cwd pkgFrontend] "npm" ["install"]
    command_ [Cwd pkgFrontend] "npm" ["prune"]  -- remove unused dependencies

  phony "build-frontend-icons" $ do
    -- NOTE: needs to run with repo root as current directory.  when calling compile-svg.hs from
    -- inside `pkg/frontend`, despite it being a stand-alone stack script, it reads stack.yaml and
    -- gets very confused about whether to use ghcjs or not.
    command_ [] "./scripts/compile-svg.hs" ((pkgFrontend </>) <$> ["images/icon", "src/Refine/Frontend/Icon/Svg/Internal.hs"])

  phony "build-frontend-trans" $ do
    command_ [] "./scripts/i18n.hs" []

  phony "build" $ do
    -- for building everything, we only need to go to backend and frontend.  prelude and common are
    -- compiled in both (two different compilers), and tested (as non-extra deps in stack.yaml) in
    -- backend.
    need ["build-backend", "build-frontend"]

  phony "build-optimal" $ do
    need ["clean"]
    stackBuildOptimal pkgBackend
    need ["build-frontend-optimize"]

  phony "hlint-prelude" $ do
    hlintPackage pkgPrelude

  phony "hlint-common" $ do
    hlintPackage pkgCommon

  phony "hlint-backend" $ do
    hlintPackage pkgBackend
    hlintPath [] "pkg/backend/app/Main.hs"

  phony "hlint-frontend" $ do
    hlintPackage pkgFrontend
    hlintPath [] "pkg/frontend/app/Main.hs"
    hlintPath [] "./pkg/frontend/styleguide"

  phony "hlint" $ do
    need ["hlint-prelude", "hlint-common", "hlint-backend", "hlint-frontend"]
    hlintPath [] "./accept"
    hlintPath [] "./scripts"

  phony "style-check" $ do
    command_ [] "./scripts/style-check.hs" []

  phony "hlint-all" $ do
    need ["hlint", "build-frontend-trans", "style-check"]


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
