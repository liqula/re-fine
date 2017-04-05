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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Contribution.StoreSpec where

import           Data.Int
import qualified Data.Map as Map
import           Test.Hspec

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Store
import           Refine.Frontend.Contribution.Types


cnid :: Int64 -> ContributionID
cnid = ContribIDNote . ID


spec :: Spec
spec = do
  describe "markPositionsUpdate" $ do
    context "old value does not exist" $ do
      it "stores new mark position." $ do
        let markPositions  = MarkPositions mempty mempty
            addEvent = ScheduleAddMarkPosition (cnid 1) (MarkPosition 300 320)
            markPositions' = MarkPositions mempty (Map.singleton (cnid 1) (MarkPosition 300 320))
        markPositionsUpdate addEvent markPositions `shouldBe` markPositions'

    context "old value exists" $ do
      let check state action state' = do
            let markPositions  = MarkPositions mempty (Map.singleton (cnid 1) state)
                addEvent       = ScheduleAddMarkPosition (cnid 1) action
                markPositions' = MarkPositions mempty (Map.singleton (cnid 1) state')
            markPositionsUpdate addEvent markPositions `shouldBe` markPositions'

      it "keeps min of top and max of bottom offset." $ do
        check (MarkPosition 300 500) (MarkPosition 350 550) (MarkPosition 300 550)  -- higher / higher
        check (MarkPosition 300 500) (MarkPosition 250 550) (MarkPosition 250 550)  -- lower / higher
        check (MarkPosition 300 500) (MarkPosition 350 400) (MarkPosition 300 500)  -- higher / lower
        check (MarkPosition 300 500) (MarkPosition 250 400) (MarkPosition 250 500)  -- lower / lower
