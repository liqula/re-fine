{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.WebSocketSpec where

import Refine.Backend.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import Network.WebSockets
import System.Random
import Test.Hspec

import Refine.Common.Types as Common


verbose :: Bool
verbose = True


sendMessage :: HasCallStack => Connection -> ToServer -> IO ()
sendMessage conn msg = sendTextData conn (cs $ encode msg :: ST)

receiveMessage :: HasCallStack => Connection -> IO ToClient
receiveMessage conn = receiveData conn <&> fromMaybe (error "websockets json decoding error") . decode

runWS :: HasCallStack => (Connection -> IO a) -> IO a
runWS action = do
  let host :: String = "localhost"
      port :: Int    = 3000
  runClient host port "/" action


throttle :: IO ()
throttle = threadDelay =<< randomRIO (1, 1000000)

stresser :: Int -> IO (Async Int)
stresser rounds = async $ do
  TCGreeting cid <- runWS $ \conn ->
    sendMessage conn (TSGreeting Nothing) >> receiveMessage conn
  forM_ [0..rounds] $ \_ -> runWS $ \conn -> do
    when verbose $ hPutStrLn stderr ("<" <> show cid <> ">")
    sendMessage conn (TSGreeting (Just cid))
    throttle
    TCRestrictKeys [] <- receiveMessage conn
    throttle
    sendMessage conn (TSLogin (Login "nobody" mempty))
    _ <- receiveMessage conn
    when verbose $ hPutStrLn stderr ("</" <> show cid <> ">")
  pure cid

stressers :: Int -> Int -> IO ()
stressers agents rounds = do
  as :: [Async Int] <- forM [0..agents] . const $ stresser rounds
  result <- show <$> wait `mapM` as
  when verbose $ hPutStrLn stderr result
  length result `shouldNotBe` 0


spec :: Spec
spec = do
  it "connects many times" $ do
    pendingWith "this only works if you run the backend server separately from the test suite."
    stressers 100 3

  it "works with many concurrent, high-volume sessions" $ do
    pending
