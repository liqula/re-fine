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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.TypesSpec where

import           Data.Map (Map)
import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck.Instances ()

import Refine.Common.Test.HttpApiData
import Refine.Common.Types


spec :: Spec
spec = parallel $ do
  describe "FromHttpApiData and ToHttpApiData are inverses" $ do
    fromAndToHttpApiDataAreInverses (h :: HttpApiGen (ID ()))

  roundtripSpecs (Proxy @(ID Int))
  roundtripSpecs (Proxy @(ID (Maybe Int)))

  -- test {From,To}JSONKey instances
  roundtripSpecs (Proxy @(Map (ID Int) Int))
  roundtripSpecs (Proxy @(Map (ID (Maybe Int)) Int))

  roundtripSpecs (Proxy @L10)
