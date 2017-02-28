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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.StoreSpec where

import Control.Lens ((^.))
import Test.Hspec

import           React.Flux (transform)

import Refine.Common.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Frontend.Store ()


instance Arbitrary (ID VDoc) where
    arbitrary = ID <$> arbitrary


spec :: Spec
spec = do
  describe "Store" $ do
    describe "transform" $ do
      context "gsVDocList" $ do
        it "integrates the loaded document list into the store" $ do
          let list = [ID 3]
          result <- transform (LoadedDocumentList list) emptyGlobalState
          result ^. gsVDocList `shouldBe` Just list

      context "gsScreenState" $ do
        it "adds the header height to the state" $ do
          newState <- transform (ScreenAction (AddHeaderHeight 64)) emptyGlobalState
          _ssHeaderHeight (_gsScreenState newState) `shouldBe` (64 :: Int)
