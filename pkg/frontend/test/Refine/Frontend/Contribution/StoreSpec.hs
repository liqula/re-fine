{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Contribution.StoreSpec where

import qualified Data.Map as Map
import           Test.Hspec

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Store
import           Refine.Frontend.Contribution.Types


spec :: Spec
spec = do
  describe "markPositionsUpdate" $ do
    context "old value does not exist" $ do
      it "stores new mark position." $ do
        let markPositions  = MarkPositions mempty
            markPositions' = MarkPositions $ Map.singleton (ID 1) (200, 100)
            addEvent = AddMarkPosition (ID 1) 200 100
        markPositionsUpdate addEvent markPositions `shouldBe` markPositions'

    context "old value exists, old offset is greater or equal to current" $ do
      it "stores new mark position." $ do
        let markPositions  = MarkPositions $ Map.singleton (ID 1) (300, 80)
            markPositions' = MarkPositions $ Map.singleton (ID 1) (200, 100)
            addEvent = AddMarkPosition (ID 1) 200 100
        markPositionsUpdate addEvent markPositions `shouldBe` markPositions'

    context "old value exists, old offset is smaller than current" $ do
      it "discards new mark position." $ do
        let markPositions  = MarkPositions $ Map.singleton (ID 1) (100, 80)
            addEvent = AddMarkPosition (ID 1) 200 100
        markPositionsUpdate addEvent markPositions `shouldBe` markPositions
