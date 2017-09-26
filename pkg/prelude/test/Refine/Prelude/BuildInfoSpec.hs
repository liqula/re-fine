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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Prelude.BuildInfoSpec
where

import Test.Hspec

import Refine.Prelude.BuildInfo
import Refine.Prelude.BuildInfo.TH


spec :: Spec
spec = do
  describe "gitCommitHashIO" $ do
    it "returns something of length 8" $ do
      h <- gitCommitHashIO
      length h `shouldNotBe` 0
      pending
      length h `shouldBe` 8

  describe "gitBuildTimeIO" $ do
    it "returns something of length 30" $ do
      h <- gitBuildTimestampIO
      length h `shouldNotBe` 0
      pending
      h `shouldContain` ("UTC" :: String)

  describe "gitCommitHash" $ do
    it "returns something of length 8" $ do
      length gitCommitHash `shouldNotBe` 0
      pending
      length gitCommitHash `shouldBe` 8

  describe "gitBuildTime" $ do
    it "returns something of length 30" $ do
      length gitBuildTimestamp `shouldNotBe` 0
      pending
      gitBuildTimestamp `shouldContain` ("UTC" :: String)
