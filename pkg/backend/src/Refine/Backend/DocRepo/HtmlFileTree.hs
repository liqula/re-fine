{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

import           Control.Exception (throwIO, ErrorCall(ErrorCall))
import           Control.Monad ((>=>))
import           Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty as Aeson
import           Data.List (partition, sort)
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import           Data.Tree
import           System.Directory
import           System.IO.Temp (createTempDirectory)
import           Text.HTML.Parser

import Refine.Common.Types.VDoc


-- | Compose 'htmlToFileTree', 'writeFileTree'.
writeHtml :: FilePath -> VDocVersion 'HTMLCanonical -> IO FilePath
writeHtml rootPath vers = do
  repoPath <- createTempDirectory rootPath ".repo"
  writeFileForest repoPath $ htmlToFileForest vers
  pure repoPath

-- | Compose 'readFileTree', 'htmlFromFileTree'.
readHtml :: FilePath -> IO (VDocVersion 'HTMLCanonical)
readHtml = fmap htmlFromFileForest . readFileForest


-- | The children order is @[TAGNAME, ATTR, SIBLINGORDER, ... (html children, in their html order)]@.
htmlToFileForest :: VDocVersion 'HTMLCanonical -> FileForest
htmlToFileForest (VDocVersion forest) = doSiblings forest
  where
    doSiblings :: Forest Token -> FileForest
    doSiblings = siblingOrderToFile . zipWith doSibling [1..]

    doSibling :: Int -> Tree Token -> FileTree
    doSibling _fileName (Node tag@(TagOpen name attrs) children) =
        Directory (mkDirName tag attrs)
                  (File "TAGNAME" name : attrsToFile attrs <> doSiblings children)
    doSibling _fileName (Node tag@(TagSelfClose name attrs) []) =
        Directory (mkDirName tag attrs)
                  (File "TAGNAME" name : attrsToFile attrs)
    doSibling fileName (Node (ContentText txt) []) = File ("s" <> show fileName) txt
    doSibling _ bad = error $ "htmlToFileForest: non-canonical input: " <> show bad

htmlFromFileForest :: FileForest -> VDocVersion 'HTMLCanonical
htmlFromFileForest = VDocVersion . go
  where
    go :: FileForest -> Forest Token
    go children = case parseFileForest children of
      (Nothing, children') -> children'
      bad                  -> error $ "htmlFromFileForest: " <> show bad

    -- Reads a list of 'FileTree's and reads out the special files TAGNAME, ATTRS, and SIBLINGORDER.
    -- Returns a tag (if found), and the non-special file trees in that order.
    parseFileForest :: FileForest -> (Maybe Token, Forest Token)
    parseFileForest forest = (mTagToken, parseFileTree <$> forestFinal)
      where
        (tagnamefile, forest')   = partition (\case (File "TAGNAME" _)      -> True; _ -> False) forest
        (attrsfile,   forest'')  = partition (\case (File "ATTRS" _)        -> True; _ -> False) forest'
        (orderfile,   forest''') = partition (\case (File "SIBLINGORDER" _) -> True; _ -> False) forest''

        mTagToken = case (tagnamefile, attrsfile) of
          ([],                       [])                   -> Nothing
          ([File "TAGNAME" tagname], [File "ATTRS" attrs]) -> Just $ TagOpen tagname (attrsFromFile attrs)
          bad -> error $ "htmlFromFileForest: " <> show bad

        forestFinal = case (orderfile, forest''') of
          ([], [])                            -> []
          ([File "SIBLINGORDER" order], _:_) -> siblingOrderFromFile order forest'''
          bad -> error $ "htmlFromFileForest: " <> show bad

    parseFileTree :: FileTree -> Tree Token
    parseFileTree (File _ contents)      = Node (ContentText contents) []
    parseFileTree (Directory _ contents) = case parseFileForest contents of
      (Just tag, children) -> Node tag children
      bad@(Nothing,  _)    -> error $ "htmlFromFileForest: " <> show bad


-- | Attributes are flattened out in tuples because (a) 'Attr' does not have a 'ToJSON' instance,
-- and (b) if it was serialized into an object, it wouldn't be deterministic (the keys have no
-- order).
attrsToFile :: [Attr] -> FileForest
attrsToFile attrs = [ File "ATTRS" $ showAttrs attrs | not $ null attrs ]

showAttrs :: [Attr] -> ST
showAttrs = cs . Aeson.encodePretty . fmap (\(Attr k v) -> (k, v))

-- | Inverse of 'attrsToFile' (operates directly on the 'File' content to make the function total).
attrsFromFile :: ST -> [Attr]
attrsFromFile = fmap (uncurry Attr) . (\(Just v) -> v) . Aeson.decode . cs

mkDirName :: Token -> [Attr] -> FilePath
mkDirName _tag (Attr "data-uid" v : _) = "u" <> cs v
mkDirName tag  (_ : xs)                = mkDirName tag xs
mkDirName tag  []                      = error $ "mkDirName: no data-uid attr in tag " <> show tag

siblingOrderToFile :: FileForest -> FileForest
siblingOrderToFile []       = []
siblingOrderToFile fs@(_:_) = File "SIBLINGORDER" (cs . unlines $ _fileName <$> fs) : fs

-- | Inverse of 'siblingOrderToFile' (operates directly on the 'File' content to make the function
-- total).  Discards all files not mentioned in the order.
siblingOrderFromFile :: ST -> FileForest -> FileForest
siblingOrderFromFile (fmap cs . ST.lines -> order) fs =
  mconcat $ (`filter` fs) . (\name tree -> _fileName tree == name) <$> order


data FileTree =
    File
      { _fileName         :: FilePath
      , _fileContent      :: ST
      }
  | Directory
      { _fileName         :: FilePath
      , _directoryContent :: FileForest
      }
  deriving (Eq, Ord, Show, Read)

type FileForest = [FileTree]


writeFileForest :: FilePath -> FileForest -> IO ()
writeFileForest rootPath = writeFileTree rootPath . Directory "."

writeFileTree :: FilePath -> FileTree -> IO ()
writeFileTree rootPath ft = withCurrentDirectory rootPath $ do
  case ft of
    File fp content -> ST.writeFile fp content
    Directory fp children -> createDirectoryIfMissing True fp >> (writeFileTree fp `mapM_` children)

readFileForest :: FilePath -> IO [FileTree]
readFileForest = readFileTree >=> stripTopLevelDir
  where
    stripTopLevelDir (Directory _ fs) = pure fs
    stripTopLevelDir bad = throwIO . ErrorCall $ "readFileForest: " <> show bad

readFileTree :: FilePath -> IO FileTree
readFileTree rootPath = do
  isdirectory <- doesDirectoryExist rootPath
  if isdirectory
    then withCurrentDirectory rootPath $ do
      children <- filter (`notElem` [".", ".."]) <$> getDirectoryContents "."
      Directory rootPath . sort <$> (readFileTree `mapM` children)
    else
      File rootPath <$> ST.readFile rootPath
