#!/usr/bin/env stack
{- stack --resolver lts-7.15 --install-ghc runghc
    --package string-conversions
    --package wai
    --package warp

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
import System.Environment
import System.Process

main :: IO ()
main = do
  [read -> port] <- getArgs
  putStrLn $ "running refresh trigger on port " <> show port

  mvar <- newMVar Nothing
  bump mvar
  run port (\_req respond -> bump mvar >> respond (responseLBS status201 [] "refreshed!"))

bump :: MVar (Maybe ProcessHandle) -> IO ()
bump mvar = modifyMVar_ mvar $ fmap Just . restart

restart :: Maybe ProcessHandle -> IO ProcessHandle
restart mpid = do
  (\pid -> terminateProcess pid >> waitForProcess pid) `mapM_` mpid
  callProcess "git" ["checkout", "master"]
  callProcess "git" ["pull"]
  spawnProcess "./scripts/build-staging.sh" []
