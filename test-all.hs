#!/usr/bin/env stack
{- stack --resolver lts-7.14 --install-ghc runghc
    --package directory
    --package turtle

    --

    -XBangPatterns
    -XOverloadedStrings
    -XScopedTypeVariables
    -XTupleSections
    -XViewPatterns

    -Wall

-}

import           Data.Monoid
import           Data.List
import           System.Directory
import           System.Exit
import           System.Process

-- TODO: we need to use something that does better reporting while executing.  shake, probably?

packages :: [FilePath]
packages = ["refine-prelude", "refine-common", "refine-backend", "refine-frontend"]

main :: IO ()
main = do
  ExitSuccess <- system "echo stack version: && stack --version"
  ExitSuccess <- system "stack --resolver lts-6.27 setup && stack --resolver lts-6.27 install hlint"
  ExitSuccess <- system $ "stack setup && for i in " <> intercalate " " packages <> "; do stack test --fast $i; done"
  hlintPkg `mapM_` packages

setupPkg :: FilePath -> IO ()
setupPkg target = withCurrentDirectory target $ do
  putStrLn $ "\n\n>>> " <> target <> " [setup]\n\n"
  ExitSuccess <- system "stack setup && stack install --fast --test --dependencies-only"  -- FUTUREWORK: --coverage --bench
  pure ()

hlintPkg :: FilePath -> IO ()
hlintPkg target = do
  ExitSuccess <- system $ "stack exec -- ~/.local/bin/hlint --hint=./refine-prelude/HLint.hs ./" <> target <> "/src ./" <> target <> "/test"
    -- FIXME: run ./app by hlint as well if available.
    -- FUTUREWORK: use --refactor to apply suggestions to working copy (requires apply-refact package).
  pure ()
