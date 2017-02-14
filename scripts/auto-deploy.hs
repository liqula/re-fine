#!/usr/bin/env stack
{- stack --resolver lts-7.15 --install-ghc runghc
    --package string-conversions
    --package wai
    --package warp
    --package executable-path

    --

    -XDeriveDataTypeable
    -XExistentialQuantification
    -XGeneralizedNewtypeDeriving
    -XOverloadedStrings
    -XPackageImports
    -XRecordWildCards
    -XScopedTypeVariables
    -XStandaloneDeriving
    -XViewPatterns

    -Wall
-}

import Control.Concurrent.MVar
import Control.Exception
import Data.String.Conversions
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment
import System.Environment.Executable
import System.FilePath
import System.Process
import Text.Read


main :: IO ()
main = do
  setProperCurrentDirectory
  args <- getArgs
  case args of
    [readMaybe . cs -> Just port] -> do
      putStrLn $ "running upgrade trigger on port " <> show port
      mvar <- newMVar Nothing
      bump mvar
      let resp = responseLBS status201 [] "server updated and upgraded!\n"
          app _req respond = bump mvar >> respond resp
      run port app

    ["--run-once"] -> do
      buildFast >>= waitForProcess >> pure ()

    _ -> do
      throwIO $ ErrorCall "bad arguments."

setProperCurrentDirectory :: IO ()
setProperCurrentDirectory = do
  RunGHC ((</> "..") . takeDirectory -> workingDir) <- getScriptPath
  putStrLn $ "setting working directory to " <> show workingDir
  setCurrentDirectory workingDir

bump :: MVar (Maybe ProcessHandle) -> IO ()
bump mvar = modifyMVar_ mvar $ fmap Just . upgrade

upgrade :: Maybe ProcessHandle -> IO ProcessHandle
upgrade mpid = do
  (\pid -> terminateProcess pid >> waitForProcess pid) `mapM_` mpid
  callProcess "git" ["checkout", "master"]
  callProcess "git" ["pull"]
  buildThorough

buildThorough :: IO ProcessHandle
buildThorough = do
  callProcess "./build" ["setup"]
  callProcess "./build" ["clean"]
  buildFast

buildFast :: IO ProcessHandle
buildFast = do
  callProcess "./build" ["build-backend", "build-frontend"]
  withCurrentDirectory "pkg/frontend" $ callProcess "npm" ["run", "build"]
  withCurrentDirectory "pkg/backend"  $ spawnProcess "stack" ["exec", "--", "refine", "server.conf"]
