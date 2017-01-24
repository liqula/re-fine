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

import Data.String.Conversions
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


data FileTree = File FilePath ST | Directory FilePath [FileTree]
  deriving (Eq, Show)

writeFileTree :: FilePath -> FileTree -> IO ()
writeFileTree _rootPath _ft = undefined

readFileTree :: FilePath -> IO FileTree
readFileTree _rootPath = undefined
