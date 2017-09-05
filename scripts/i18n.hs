#!/usr/bin/env stack
{- stack --resolver lts-8.11 --install-ghc runghc
    --package foldl
    --package regex-posix
    --package string-conversions
    --package system-filepath
    --package turtle

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import           Data.List (foldl', sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Filesystem.Path.CurrentOS as Path
import           Prelude hiding (FilePath)
import           System.Environment
import           System.Exit
import           Text.Read (readMaybe)
import           Text.Regex.Posix
import           Turtle hiding (f)


usage :: ExitCode -> IO ()
usage exitCode = do
  unitTestMatchTKeys
  pname <- getProgName
  putStrLn $ unlines
    [ unwords ["usage:", pname, "<source_tree>", "<trans_module_path>", "<trans_module_name>", "<po*_path>"]
    , ""
    , "Generates \"*.Trans\" modules and PO, POT files for internationalization.  The purpose of the"
    , "\"*.Trans\" modules is to make it explicit what language keys we have, which should make"
    , "it harder to introduce spelling errors."
    , ""
    , "<source_tree> can be any sub-tree of the target code base, including the entire code base."
    , "To keep things simple, <trans_module_path>, <trans_module_name> both have to be specified"
    , "somewhat redundantly (example: src/Frontend/Views.hs and Frontend.Views), and <po*_file>"
    , "must be absolute paths."
    , ""
    , "Existing PO files under <po*_path> will be updated (new t-keys added with empty translation,"
    , "unused ones removed)."
    , ""
    , "Note that updating PO files is only guaranteed to work on PO files generated by this script."
    , "When translating, best not to change anything outside of the translation texts!"
    ]
  exitWith exitCode


transModDeps :: [ST]
transModDeps = ["Refine.Common.Types.Translation", "Data.Text"]


main :: IO ()
main = do
  sh assertWorkingCopyClean
  unitTestMatchTKeys
  args <- getArgs
  case args of
    [sourceTree, transModuleFile, transModuleName, poDir]
       -> main' (cs sourceTree) (cs transModuleFile) (cs transModuleName) (cs poDir)
    [] -> main' "pkg/frontend/src/" "pkg/frontend/src/Refine/Frontend/TKey.hs" "Refine.Frontend.TKey" "po/"
    _ -> usage $ ExitFailure 1
  sh yellIfChanged

main' :: FilePath -> FilePath -> ST -> FilePath -> IO ()
main' sourceTree transModuleFile transModuleName poDir = do
  sourceFiles <- filter ((== Just "hs") . extension) <$> (lstree sourceTree `fold` Fold.list)
  tKeyCalls   <- mkTKeyCalls . mconcat <$> (getTKeyCallsIO `mapM` sourceFiles)
  ST.writeFile (Path.encodeString transModuleFile) $ createTransModule transModuleName tKeyCalls
  ST.writeFile (Path.encodeString $ poDir </> "template.pot") $ createPotFile tKeyCalls
  updatePoFilesIO poDir tKeyCalls

yellIfChanged :: Shell ()
yellIfChanged = do
  dirty <- dirtyFiles
  unless (null dirty) $ do
    echo "\n"
    echo "NOTE: ./scripts/i18n.hs has updated the translation tables."
    echo "If this happens in the ci and you get an error, run ./scripts/i18n.hs without"
    echo "arguments and review / commit the resulting changes."
    echo "\n"


-- * collect

data TKeyCall = TKeyCall FilePath Int ST
  deriving (Eq, Show)

getTKeyCallsIO :: FilePath -> IO [TKeyCall]
getTKeyCallsIO filepath = getTKeyCalls filepath <$> ST.readFile (Path.encodeString filepath)

getTKeyCalls :: FilePath -> ST -> [TKeyCall]
getTKeyCalls filepath = mconcat . fmap (uncurry go) . zip [1..] . ST.lines
  where
    go :: Int -> ST -> [TKeyCall]
    go num line = TKeyCall filepath num . cs <$> matchTKeys (cs line)

matchTKeys :: SBS -> [SBS]
matchTKeys line = case mrSubList res of
  []    -> []
  [key] -> key : matchTKeys (mrAfter res)
  bad   -> error $ "getTKeyCalls: impossible regex match: " <> show bad
  where
    res :: MatchResult SBS
    res = line =~ ("__ (\\w\\w\\w+)" :: SBS)

unitTestMatchTKeys :: IO ()
unitTestMatchTKeys = sequence_
  [ ($ pure ()) . assert $ matchTKeys "(__ some_keyIWU)" == ["some_keyIWU"]
  , ($ pure ()) . assert $ matchTKeys "..  (__ some_keyIWU) .. (__ WER) ..." == ["some_keyIWU", "WER"]
  , ($ pure ()) . assert $ matchTKeys "            ] (elemText $ __ add_a_comment)" == ["add_a_comment"]
  , ($ pure ()) . assert $ matchTKeys "       elemText $ __ add1_a_comment" == ["add1_a_comment"]
  , ($ pure ()) . assert $ matchTKeys "       elemText $ __ add2_a_comment," == ["add2_a_comment"]
  , ($ pure ()) . assert $ matchTKeys "       elemText $ __ add3_a_comment " == ["add3_a_comment"]
  ]


-- * generate

type TKeyCalls = Map ST [(FilePath, Int)]
type TKeyCallsWithTrans = Map ST ([(FilePath, Int)], ST)

mkTKeyCalls :: [TKeyCall] -> TKeyCalls
mkTKeyCalls = foldl' go mempty
  where
    go m (TKeyCall sp sl key) = Map.alter (alt (sp, sl)) key m
    alt loc = Just . maybe [loc] (loc:)

createTransModule :: ST -> TKeyCalls -> ST
createTransModule name tKeyCalls = ST.unlines $
    ["module " <> name <> " where", ""] <>
    (("import " <>) <$> transModDeps) <> [""] <>
    ["{-# ANN module \"HLint: ignore Use camelCase\" #-}", ""] <>
    (go <$> Map.keys tKeyCalls)
  where
    go key = ST.unlines
      [ key <> " :: TKey"
      , key <> " = TKey $ pack \"" <> key <> "\""
      ]

createPotFile :: TKeyCalls -> ST
createPotFile = createPoFile . ((, "") <$>)


updatePoFilesIO :: FilePath -> TKeyCalls -> IO ()
updatePoFilesIO poDir tKeyCalls =
  mapM_ (`updatePoFileIO` tKeyCalls) . filter ((== Just "po") . extension) =<< (lstree poDir `fold` Fold.list)

updatePoFileIO :: FilePath -> TKeyCalls -> IO ()
updatePoFileIO (Path.encodeString -> poFile) tKeyCalls = do
  !poContent <- (`updatePoFile` tKeyCalls) <$> ST.readFile poFile
  ST.writeFile poFile poContent

updatePoFile :: ST -> TKeyCalls -> ST
updatePoFile poContent tKeyCalls = createPoFile . merge tKeyCalls . readPoFile $ poContent
  where
    merge :: TKeyCalls -> TKeyCallsWithTrans -> TKeyCallsWithTrans
    merge clls trns = addTrans `Map.mapWithKey` clls
      where
        addTrans :: ST -> [(FilePath, Int)] -> ([(FilePath, Int)], ST)
        addTrans tkey loc = (loc, maybe "###" snd (Map.lookup tkey trns))

readPoFile :: ST -> TKeyCallsWithTrans
readPoFile = s0 . ST.lines
  where
    s0 :: [ST] -> TKeyCallsWithTrans
    s0 ("" : lns) = s0 lns
    s0 lns        = s1 [] lns

    s1 :: [(FilePath, Int)] -> [ST] -> TKeyCallsWithTrans
    s1 locs (ln : lns) = if "#: " `ST.isPrefixOf` ln
      then s1 (parseLoc ln : locs) lns
      else s2 (reverse locs) (ln : lns)
    s1 [] [] = mempty
    s1 (_:_) [] = error "readPoFile: parse error (file ends with source locations)."

    s2 :: [(FilePath, Int)] -> [ST] -> TKeyCallsWithTrans
    s2 locs ((ST.splitAt 6 -> ("msgid ", readMaybe . cs -> Just tkey)) : lns) = s3 (locs, tkey) lns
    s2 _ _ = error "readPoFile: parse error (file ends with broken/incomplete entry at msgid)."

    s3 :: ([(FilePath, Int)], ST) -> [ST] -> TKeyCallsWithTrans
    s3 (locs, key) ((ST.splitAt 7 -> ("msgstr ", readMaybe . cs -> Just trans)) : lns) =
        Map.insert key (locs, trans) $ s0 lns
    s3 _ _ = error "readPoFile: parse error (file ends with broken/incomplete entry at msgstr)."

parseLoc :: ST -> (FilePath, Int)
parseLoc line = case mrSubList ((cs line :: SBS) =~ ("^#: (.+):(.+)$" :: SBS) :: MatchResult SBS) of
  [cs -> sourceFile, read . cs -> sourceLine] -> (sourceFile, sourceLine)
  bad   -> error $ "parseLoc: parse error: " <> show (line, bad)

createPoFile :: TKeyCallsWithTrans -> ST
createPoFile = ST.unlines . fmap go . Map.toAscList
  where
    go :: (ST, ([(FilePath, Int)], ST)) -> ST
    go (key, (locs, trans)) = ST.unlines $ (showLoc <$> sort locs) <>
      [ "msgid \"" <> key <> "\""
      , "msgstr " <> (cs . show $ trans)  -- (not fast, but escapes '"'.)
      ]

    showLoc :: (FilePath, Int) -> ST
    showLoc (sourcePath, sourceLine) = cs $ "#: " <> Path.encodeString sourcePath <> ":" <> show sourceLine


-- * should go into separate package

instance ConvertibleStrings ST Path.FilePath where
  convertString = Path.fromText

instance ConvertibleStrings Path.FilePath ST where
  convertString = either (error . show) id . Path.toText

instance ConvertibleStrings String Path.FilePath where
  convertString = cs @ST . cs

instance ConvertibleStrings Path.FilePath String where
  convertString = cs @ST . cs

instance ConvertibleStrings SBS Path.FilePath where
  convertString = cs @ST . cs

instance ConvertibleStrings Path.FilePath SBS where
  convertString = cs @ST . cs


-- * git (section copied from ./style-check.hs, keep in sync manually)

dirtyFiles :: Shell [GitStatus]
dirtyFiles = do
  let interesting (GitStatus _ Untracked _) = False
      interesting (GitStatus _ Ignored _)   = False
      interesting _                         = True
  mconcat <$> (filter interesting <$> gitStatus) `fold` Fold.list

assertWorkingCopyClean :: Shell ()
assertWorkingCopyClean = do
  gs <- dirtyFiles
  unless (null gs) $ do
    echo "this script can only be run on a clean working copy!"
    exit $ ExitFailure 1

gitStatus :: Shell [GitStatus]
gitStatus = fmap parse . ST.lines . lineToText <$> inshell "git status --porcelain ." Turtle.empty
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
