{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import System.Environment (getArgs, getProgName)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)

import Refine.Backend.App.Access
import Refine.Backend.App.Core (appLog)
import Refine.Backend.App.MigrateDB (initializeDB)
import Refine.Backend.App.Smtp (checkSendMail)
import Refine.Backend.Config
import Refine.Backend.Logger
import Refine.Backend.Server

help :: IO ()
help = do
  progname <- getProgName
  putStrLn $ unlines
    [ "Usage: " <> progname <> " [--help | --help-init | --init <FILE>] [cfg]"
    , "  --help         Print this help"
    , "  --help-init    Print help on creating initial content in the database"
    , "  --init <FILE>  Create initial content in the database"
    , ""
    , "Without parameter, the server starts, if the config file is given."
    ]

runInitDB :: FilePath -> Maybe FilePath -> IO ()
runInitDB contentPath configPath = do
  cfg <- initConfig configPath
  content <- readCliCreate contentPath
  runCliAppCommand cfg $ do
    unsafeAsGod $ initializeDB content
    appLog LogInfo `mapM_` notifyOfDBDump cfg

startServer :: Maybe FilePath -> IO ()
startServer configPath = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  cfg <- initConfig configPath
  checkSendMail cfg
  startBackend cfg

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--help" : _                            -> help
    "--help-init" : _                       -> helpCliCreate
    "--init" : file : cfg | length cfg <= 1 -> runInitDB file (listToMaybe cfg)
    cfg                   | length cfg <= 1 -> startServer (listToMaybe cfg)
    bad                                     -> putStrLn ("\n\nbad arguments: " <> show bad <> "\n") >> help
