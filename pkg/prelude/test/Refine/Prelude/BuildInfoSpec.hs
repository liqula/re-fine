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
      length h `shouldBe` 8

  describe "gitBuildTimeIO" $ do
    it "returns something of length 30" $ do
      h <- gitBuildTimestampIO
      h `shouldContain` ("UTC" :: String)

  describe "gitCommitHash" $ do
    it "returns something of length 8" $ do
      length gitCommitHash `shouldBe` 8

  describe "gitBuildTime" $ do
    it "returns something of length 30" $ do
      gitBuildTimestamp `shouldContain` ("UTC" :: String)
