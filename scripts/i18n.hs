#!/usr/bin/env stack
{- stack --resolver lts-7.15 --install-ghc runghc
    --package foldl
    --package regex-posix
    --package string-conversions
    --package system-filepath
    --package turtle

    --

    -XDeriveDataTypeable
    -XExistentialQuantification
    -XGeneralizedNewtypeDeriving
    -XLambdaCase
    -XNoImplicitPrelude
    -XOverloadedStrings
    -XPackageImports
    -XRecordWildCards
    -XScopedTypeVariables
    -XStandaloneDeriving
    -XViewPatterns

    -Wall

-}

import qualified Control.Foldl as Fold
import           Data.List (foldl', sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)
import           System.Environment
import           System.Exit
import           Text.Regex.Posix
import           Turtle hiding (f)


usage :: ExitCode -> IO ()
usage exitCode = do
  pname <- getProgName
  putStrLn $ unlines
    [ unwords ["usage:", pname, "<source_tree>", "<trans_module_path>", "<trans_module_name>", "<POT_file>"]
    , ""
    , "Generates \"*.Trans\" modules and POT files for internationalization.  The purpose of the"
    , "\"*.Trans\" modules is to make it explicit what language keys we have, which should make"
    , "it harder to introduce spelling errors."
    , ""
    , "<source_tree> can be any sub-tree of the target code base, including the entire code base."
    , "To keep things simple, <trans_module_path>, <trans_module_name> both have to be specified"
    , "somewhat redundantly (example: src/Frontend/Views.hs and Frontend.Views), and <POT_file>"
    , "must be absolute paths."
    , ""
    ]
  exitWith exitCode


transModDeps :: [ST]
transModDeps = ["Refine.Common.Translations"]


main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceTree, transModuleFile, transModuleName, potFile]
      -> main' (fromString sourceTree) (fromString transModuleFile) (cs transModuleName) (fromString potFile)
    _ -> usage $ ExitFailure 1


main' :: FilePath -> FilePath -> ST -> FilePath -> IO ()
main' sourceTree transModuleFile transModuleName potFile = do
  sourceFiles <- filter ((== Just "hs") . extension) <$> (lstree sourceTree `fold` Fold.list)
  tKeyCalls   <- mkTKeyCalls . mconcat <$> (getTKeyCallsIO `mapM` sourceFiles)
  ST.writeFile (encodeString transModuleFile) $ createTransModule transModuleName tKeyCalls
  ST.writeFile (encodeString potFile)         $ createPotFile                     tKeyCalls


-- * collect

data TKeyCall = TKeyCall FilePath Int ST
  deriving (Eq, Show)

getTKeyCallsIO :: FilePath -> IO [TKeyCall]
getTKeyCallsIO filepath = getTKeyCalls filepath <$> ST.readFile (encodeString filepath)

getTKeyCalls :: FilePath -> ST -> [TKeyCall]
getTKeyCalls filepath = catMaybes . fmap (uncurry f) . zip [1..] . ST.lines
  where
    f :: Int -> ST -> Maybe TKeyCall
    f num line = case mrSubList ((cs line :: SBS) =~ ("\\(__ (.*)\\)" :: SBS) :: MatchResult SBS) of  -- TODO: [a-zA-Z_].  test!
      []    -> Nothing
      [key] -> Just $ TKeyCall filepath num (cs key)
      bad   -> error $ "getTKeyCalls: impossible regex match: " <> show bad


-- * generate

type TKeyCalls = Map ST [(FilePath, Int)]

mkTKeyCalls :: [TKeyCall] -> TKeyCalls
mkTKeyCalls = foldl' go mempty
  where
    go m (TKeyCall sp sl key) = Map.alter (alt (sp, sl)) key m
    alt loc = Just . maybe [loc] (loc:)

createTransModule :: ST -> TKeyCalls -> ST
createTransModule name tKeyCalls = ST.unlines $
    ["module " <> name <> " where", ""] <>
    (("import " <>) <$> transModDeps) <> [""] <>
    (go <$> Map.keys tKeyCalls)
  where
    go key = ST.unlines
      [ key <> " :: TKey"
      , key <> " = \"" <> key <> "\""
      ]

createPotFile :: TKeyCalls -> ST
createPotFile = ST.unlines . fmap go . Map.toAscList
  where
    go :: (ST, [(FilePath, Int)]) -> ST
    go (key, locs) = ST.unlines $ (showLoc <$> sort locs) <>
      [ "msgid \"" <> key <> "\""
      , "msgstr \"\""
      ]

    showLoc :: (FilePath, Int) -> ST
    showLoc (sourcePath, sourceLine) = cs $ "#: " <> encodeString sourcePath <> ":" <> show sourceLine
