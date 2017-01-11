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


cmd :: String -> IO ()
cmd c = do
  putStrLn $ "\n>> " <> c
  ExitSuccess <- system c
  pure ()

assertStack :: IO ()
assertStack = cmd "echo -n 'stack version: ' && stack --version"

-- | hlint must be at least v1.9.39 to work with ghc8 and our language extensions.  install it
-- global to the user running this script so integration testing won't have to do it every time.
assertHlint :: IO ()
assertHlint = do
  let resolver = "nightly-2017-01-10"
      expectver = "HLint v1.9.39, (C) Neil Mitchell 2006-2016" :: String
  cmd $ "test \"`stack exec -- hlint --version`\" = " <> show expectver <> " || " <>
        "( cd / && stack install --resolver " <> resolver <> " hlint )"
  cmd "echo -n 'hlint version: ' && stack exec -- hlint --version"

-- | install hspec-discover global to the user running this script so integration testing won't have
-- to do it every time.
assertHspeDiscover :: IO ()
assertHspeDiscover = do
  let resolver = "nightly-2017-01-10"
  cmd $ "stack exec which hspec-discover >/dev/null 2>/dev/null || " <>
        "( cd / && stack install --resolver " <> resolver <> " hspec-discover )"
  cmd "echo -n 'hspec-discover path: ' && stack exec -- which hspec-discover"

main :: IO ()
main = do
  assertStack
  assertHlint
  assertHspeDiscover

  cmd "stack setup"
  cmd $ "for i in " <> intercalate " " packages <> "; do stack test --fast $i; done"
  hlintPkg `mapM_` packages

setupPkg :: FilePath -> IO ()
setupPkg target = withCurrentDirectory target $ do
  putStrLn $ "\n\n>>> " <> target <> " [setup]\n\n"
  cmd "stack setup && stack install --fast --test --dependencies-only"  -- FUTUREWORK: --coverage --bench
  pure ()

hlintPkg :: FilePath -> IO ()
hlintPkg target = do
  cmd $ "stack exec -- ~/.local/bin/hlint --hint=./refine-prelude/HLint.hs " <>
        "./" <> target <> "/src ./" <> target <> "/test"
    -- FIXME: run ./app by hlint as well if available.
    -- FUTUREWORK: use --refactor to apply suggestions to working copy (requires apply-refact package).
