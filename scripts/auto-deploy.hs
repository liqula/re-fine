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
import Data.String.Conversions
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment
import System.Process
import System.Environment.Executable
import System.FilePath


main :: IO ()
main = do
  [read -> port] <- getArgs
  RunGHC ((</> "..") . takeDirectory -> workingDir) <- getScriptPath
  putStrLn $ "setting working directory to " <> show workingDir
  setCurrentDirectory workingDir
  putStrLn $ "running refresh trigger on port " <> show port

  mvar <- newMVar Nothing
  bump mvar
  let resp = responseLBS status201 [] "server updated and restarted!\n"
      app _req respond = bump mvar >> respond resp
  run port app

bump :: MVar (Maybe ProcessHandle) -> IO ()
bump mvar = modifyMVar_ mvar $ fmap Just . restart

restart :: Maybe ProcessHandle -> IO ProcessHandle
restart mpid = do
  (\pid -> terminateProcess pid >> waitForProcess pid) `mapM_` mpid
  callProcess "git" ["checkout", "master"]
  callProcess "git" ["pull"]
  callProcess "./build" ["setup"]
  callProcess "./build" ["clean"]
  callProcess "./build" ["build-backend"]
  callProcess "./build" ["build-frontend"]
  withCurrentDirectory "pkg/frontend" $ callProcess "npm" ["run", "build"]
  withCurrentDirectory "pkg/backend"  $ spawnProcess "stack" ["exec", "--", "refine", "server.conf"]
