module Main where

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stderr)

import Refine.Backend.Config
import Refine.Backend.Server
import Refine.Backend.App.MigrateDB (createInitialDB)

help :: IO ()
help = do
  putStrLn $ unlines
    [ "Usage: re-fine [--init | --help] [cfg]"
    , "  --help  Print this help"
    , "  --init  Creates initial database"
    , ""
    , "Without parameter, the server starts, if the config file is given."
    ]

runInitDB :: Maybe FilePath -> IO ()
runInitDB configPath = do
  cfg <- initConfig configPath
  runCliAppCommand cfg createInitialDB

startServer :: Maybe FilePath -> IO ()
startServer configPath = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  cfg <- initConfig configPath
  startBackend cfg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] | arg == "--help" -> help
          | arg == "--init" -> runInitDB Nothing
    (arg:cfg) | arg == "--help" -> help
              | arg == "--init" -> runInitDB (listToMaybe cfg)
    cfg -> startServer (listToMaybe cfg)
