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
            Just errmsg -> respond $ responseLBS status500 [] ("uncaught error: " <> cs errmsg)
      run port app

    ["--run-once"] -> do
      buildFast >>= \case
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
  (\pid -> terminateProcess pid >> waitForProcess pid) `mapM_` mpid
  callProcess "git" ["checkout", "master"]
  callProcess "git" ["pull"]
  buildThorough

buildThorough :: IO (Either String ProcessHandle)
buildThorough = do
  callProcess "./build" ["setup"]
  callProcess "./build" ["clean"]
  buildFast

buildFast :: IO (Either String ProcessHandle)
buildFast = do
  callProcess "./build" ["build-backend", "build-frontend"]
  withCurrentDirectory "pkg/frontend" $ callProcess "npm" ["run", "build"]
  ph <- withCurrentDirectory "pkg/backend"  $ spawnProcess "stack" ["exec", "--", "refine", "server.conf"]
  livenessCheck >>= \case
    Nothing -> pure $ Right ph
    Just errmsg -> terminateProcess ph >> waitForProcess ph >> pure (Left errmsg)

livenessCheck :: IO (Maybe String)
livenessCheck = pure Nothing
