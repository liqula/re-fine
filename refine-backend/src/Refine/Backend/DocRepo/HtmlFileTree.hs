{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.DocRepo.HtmlFileTree where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Control.Monad ((>=>))
import Data.List (sort)
import Data.String.Conversions
import Data.Text.IO as ST
import System.Directory
import System.IO.Temp (createTempDirectory)

import Refine.Common.Types.VDoc


-- | Compose 'htmlToFileTree', 'writeFileTree'.
writeHtml :: FilePath -> VDocVersion 'HTMLCanonical -> IO FilePath
writeHtml rootPath vers = do
  repoPath <- createTempDirectory rootPath ".repo"
  writeFileTree repoPath $ htmlToFileTree vers
  pure repoPath

-- | Compose 'readFileTree', 'htmlFromFileTree'.
readHtml :: FilePath -> IO (VDocVersion 'HTMLCanonical)
readHtml = fmap htmlFromFileTree . readFileTree


-- | Take a file path 'fp' and a canonicalized vdoc version, create a directory under 'fp', unravel
-- the vdoc version into that directory and return its filepath.
htmlToFileTree :: VDocVersion 'HTMLCanonical -> FileTree
htmlToFileTree = undefined

-- | Take the file path of an unravelled vdoc version, re-ravel it and return it.
htmlFromFileTree :: FileTree -> VDocVersion 'HTMLCanonical
htmlFromFileTree = undefined


data FileTree =
    File
      { _fileName :: FilePath
      , _fileContent :: ST
      }
  | Directory
      { _fileName :: FilePath
      , _directoryContent :: [FileTree]
      }
  deriving (Eq, Ord, Show)

writeFileTree :: FilePath -> FileTree -> IO ()
writeFileTree rootPath ft = withCurrentDirectory rootPath $ do
  case ft of
    File fp content -> ST.writeFile fp content
    Directory fp children -> createDirectory fp >> (writeFileTree fp `mapM_` children)

readFileTree :: FilePath -> IO FileTree
readFileTree rootPath = do
  isdirectory <- doesDirectoryExist rootPath
  if isdirectory
    then withCurrentDirectory rootPath $ do
      children <- filter (`notElem` [".", ".."]) <$> getDirectoryContents "."
      Directory rootPath . sort <$> (readFileTree `mapM` children)
    else
      File rootPath <$> ST.readFile rootPath
