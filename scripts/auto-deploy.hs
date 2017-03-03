#!/usr/bin/env stack
{- stack --resolver lts-7.15 --install-ghc runghc
    --package executable-path
    --package string-conversions
    --package wai
    --package warp

    --

    -XDeriveDataTypeable
    -XExistentialQuantification
    -XGeneralizedNewtypeDeriving
    -XLambdaCase
    -XOverloadedStrings
    -XPackageImports
    -XRecordWildCards
    -XScopedTypeVariables
    -XStandaloneDeriving
    -XViewPatterns

    -Wall
-}

import Control.Concurrent
import Control.Exception
import Data.String.Conversions
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment
import System.Environment.Executable
import System.Exit
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
      upgrade mvar >>= \case
        Nothing     -> pure ()
        Just errmsg -> error errmsg
      let app _req respond = upgrade mvar >>= \case
            Nothing     -> respond $ responseLBS status201 [] "server updated and upgraded!\n"
            Just errmsg -> respond $ responseLBS status500 [] ("uncaught error: " <> cs errmsg <> "\n")
      run port app

    ["--run-once"] -> do
      buildFast Nothing >>= \case
        Right ph    -> waitForProcess ph >> pure ()
        Left errmsg -> error errmsg

    _ -> do
      throwIO $ ErrorCall "bad arguments."

setProperCurrentDirectory :: IO ()
setProperCurrentDirectory = do
  RunGHC ((</> "..") . takeDirectory -> workingDir) <- getScriptPath
  putStrLn $ "setting working directory to " <> show workingDir
  setCurrentDirectory workingDir

upgrade :: MVar (Maybe ProcessHandle) -> IO (Maybe String)
upgrade mvar = modifyMVar mvar
    $ fmap (\case
        Right ph    -> (Just ph, Nothing)
        Left errmsg -> (Nothing, Just errmsg))
    . upgrade_

upgrade_ :: Maybe ProcessHandle -> IO (Either String ProcessHandle)
upgrade_ mpid = do
  callProcess "git" ["checkout", "master"]
  callProcess "git" ["pull"]
  buildThorough mpid

buildThorough :: Maybe ProcessHandle -> IO (Either String ProcessHandle)
buildThorough mpid = do
  callProcess "./build" ["setup"]
  callProcess "./build" ["clean"]
  buildFast mpid

buildFast :: Maybe ProcessHandle -> IO (Either String ProcessHandle)
buildFast mpid = do
  callProcess "./build" ["build-backend", "build-frontend"]
  withCurrentDirectory "pkg/frontend" $ callProcess "npm" ["run", "build"]
  (\pid -> terminateProcess pid >> waitForProcess pid) `mapM_` mpid
  ph <- withCurrentDirectory "pkg/backend"  $ spawnProcess "stack" ["exec", "--", "refine", "server.conf"]
  livenessCheck >>= \case
    Nothing -> pure $ Right ph
    Just errmsg -> terminateProcess ph >> waitForProcess ph >> pure (Left errmsg)

serverUrl :: String
serverUrl = "http://localhost:3000/"  -- FIXME: this could look into server.conf to make a more robust guess.

serverStartDelaySecs :: Int
serverStartDelaySecs = 3

livenessCheck :: IO (Maybe String)
livenessCheck = do
  threadDelay (serverStartDelaySecs * 1000 * 1000)
  status <- spawnProcess "wget" [serverUrl] >>= waitForProcess
                  -- (curl only gives you the choice between no output and correct exit code (-q) and vice versa.)
  case status of
    ExitSuccess -> pure Nothing
    _ -> pure . Just $ "server unreachable under "
                        <> show serverUrl <> " after "
                        <> show serverStartDelaySecs <> " seconds"
