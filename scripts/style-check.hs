#!/usr/bin/env stack
{- stack --resolver lts-7.15 --install-ghc runghc
    --package executable-path
    --package string-conversions
    --package system-filepath
    --package temporary
    --package turtle

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-imports #-}

import           Control.Exception (assert)
import qualified Control.Foldl as Fold
import           Control.Monad
import           Data.List (foldl', sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import           Filesystem.Path.CurrentOS hiding (empty, null)
import qualified GHC.IO
import           Prelude hiding (FilePath)
import           System.Directory
import qualified System.FilePath
import           System.Environment
import           System.Environment.Executable
import           System.Exit
import           System.IO.Temp
import           Text.Read (readMaybe)
import           Turtle hiding (f)


-- | run this on a clean working copy (no un-commited changes; untracked and ignored files are
-- allowed).  it reports *and fixes* style violations and lets you examine and commit the changes
-- when it is done.
main :: IO ()
main = sh $ do
  setProperCurrentDirectory
  fixTrailingWhitespace =<< getSourceFiles
  failOnChangedFiles

getSourceFiles :: MonadIO m => m [FilePath]
getSourceFiles = filterExt "hs" <$> getAllFiles roots
  where roots = [ "pkg" </> pkg </> topic | pkg <- ["prelude", "common", "backend", "frontend"], topic <- ["src", "test"] ]

failOnChangedFiles :: Shell ()
failOnChangedFiles = do
  let interesting (GitStatus _ Untracked _) = False
      interesting (GitStatus _ Ignored _)   = False
      interesting _                         = True

  gs <- filter interesting <$> gitStatus

  unless (null gs) $ do
    echo ".../scripts/style-check.hs has made changes to the code, which means this version contains style rule violations."
    echo "please re-run the script locally and commit the changes, or open an issue complaining about the style rules."
    echo . ST.unlines $ cs . show <$> gs
    exit $ ExitFailure 1


-- * rules

-- | both horizontally and vertically
fixTrailingWhitespace :: [FilePath] -> Shell ()
fixTrailingWhitespace files = do
  let trans = fmap ST.stripEnd . reverse . dropEmptyLines . reverse
      dropEmptyLines ("" : xs@("" : _)) = dropEmptyLines xs
      dropEmptyLines xs                 = xs
  transformFile trans `mapM_` files


-- * git

gitStatus :: Shell [GitStatus]
gitStatus = fmap parse . ST.lines <$> inshell "git status --porcelain ." empty
  where
    parse :: ST -> GitStatus
    parse line = GitStatus (parseCode ix) (parseCode wt) (cs file)
      where
        ix   = ST.take 1               line
        wt   = ST.take 1 . ST.drop 1 $ line
        file =             ST.drop 3   line

    parseCode :: ST -> GitStatusCode
    parseCode " " = Unmodified
    parseCode "M" = Modified
    parseCode "A" = Added
    parseCode "D" = Deleted
    parseCode "R" = Renamed
    parseCode "C" = Copied
    parseCode "U" = UpdatedButUnmerged
    parseCode "?" = Untracked
    parseCode "!" = Ignored
    parseCode bad = error $ "gitStatus: could not parse status code " <> show bad

data GitStatus = GitStatus GitStatusCode GitStatusCode FilePath
  deriving (Eq, Show, Ord)

data GitStatusCode =
    Unmodified
  | Modified
  | Added
  | Deleted
  | Renamed
  | Copied
  | UpdatedButUnmerged
  | Untracked
  | Ignored
  deriving (Eq, Show, Ord)


-- * helpers

withClosedSystemTempFile :: String -> (FilePath -> IO a) -> IO a
withClosedSystemTempFile template action = withSystemTempDirectory template $ action . cs . (<> "/file")

-- | (not directories)
getAllFiles :: MonadIO m => [FilePath] -> m [FilePath]
getAllFiles roots = do
  paths <- fmap mconcat $ (\r -> lstree r `fold` Fold.list) `mapM` roots
  filterM (liftIO . doesFileExist . cs) paths

filterExt :: ST -> [FilePath] -> [FilePath]
filterExt ext = filter ((== Just ext) . extension)

transformFile :: ([ST] -> [ST]) -> FilePath -> Shell ()
transformFile trans file = do
  contents :: [ST] <- input file `fold` Fold.list
  let contents' :: [ST] = trans contents
  when (contents' /= contents) . liftIO $ do
    withClosedSystemTempFile "refine.tmp" $ \file' -> do
      ST.writeFile  (cs file') $ ST.unlines contents'
      mv file' file


-- * amendments to turtle

instance ConvertibleStrings ST Filesystem.Path.CurrentOS.FilePath where
  convertString = Filesystem.Path.CurrentOS.fromText

instance ConvertibleStrings Filesystem.Path.CurrentOS.FilePath ST where
  convertString = either (error . show) id . Filesystem.Path.CurrentOS.toText

instance ConvertibleStrings String Filesystem.Path.CurrentOS.FilePath where
  convertString = cs @ST . cs

instance ConvertibleStrings Filesystem.Path.CurrentOS.FilePath String where
  convertString = cs @ST . cs


echoShow :: (MonadIO m, Show a) => a -> m ()
echoShow = echo . cs . show

echoCS :: (MonadIO m, ConvertibleStrings s ST) => s -> m ()
echoCS = echo . cs


-- * should go to a separate packge

setProperCurrentDirectory :: MonadIO m => m ()
setProperCurrentDirectory = liftIO $ do
  progName <- getProgName

  let setdir ((</> "..") . cs . System.FilePath.takeDirectory -> workingDir) = do
        putStrLn $ progName <> ": setting working directory to " <> show workingDir
        setCurrentDirectory (cs workingDir)

      setdiri = do
        wd <- getCurrentDirectory
        when (System.FilePath.takeFileName wd == "scripts") $ setCurrentDirectory ".."
        putStr "running interactively; wd: "
        putStrLn =<< getCurrentDirectory

  getScriptPath >>= \case
    Executable wd -> setdir wd
    RunGHC wd     -> setdir wd
    Interactive   -> setdiri
