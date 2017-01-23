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

module Refine.Backend.ConfigSpec where

import qualified Data.Aeson as Aeson
import           Test.Hspec

import           Refine.Backend.Config


spec :: Spec
spec = do
  describe "WarpSettings" $ do
    it "aeson encode and decode are inverses" $ do
      let check :: WarpSettings -> Expectation
          check x = Aeson.eitherDecode (Aeson.encode x) `shouldBe` Right x

      check $ WarpSettings 3000 "127.0.0.1"
      check $ WarpSettings 3000 "*"   -- means HostAny - "any IPv4 or IPv6 hostname"
      check $ WarpSettings 3000 "*4"  -- means HostIPv4 - "any IPv4 or IPv6 hostname, IPv4 preferred"
      check $ WarpSettings 3000 "!4"  -- means HostIPv4Only - "any IPv4 hostname"
      check $ WarpSettings 3000 "*6"  -- means HostIPv6@ - "any IPv4 or IPv6 hostname, IPv6 preferred"
      check $ WarpSettings 3000 "!6"  -- means HostIPv6Only - "any IPv6 hostname"
