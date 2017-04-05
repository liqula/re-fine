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

module Refine.Frontend.Contribution.QuickCreateSpec where

import Test.Hspec

import           Refine.Frontend.Contribution.QuickCreate
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Types


rangeTopFor :: Int -> Int -> Range
rangeTopFor topOffset = rangeFor 0 topOffset 0

rangePosFor :: Int -> Int -> Range
rangePosFor topOffset bottomOffset = rangeFor 0 topOffset bottomOffset 0

rangeFor :: Int -> Int -> Int -> Int -> Range
rangeFor docTopOffset topOffset bottomOffset scrollOffset =
  Range Nothing Nothing
      (OffsetFromDocumentTop docTopOffset)
      (OffsetFromViewportTop topOffset)
      (OffsetFromViewportTop bottomOffset)
      (ScrollOffsetOfViewport scrollOffset)

mh :: Int
mh = 80 -- menu height
ih :: Int
ih = 44 -- icon height
ih2 :: Int
ih2 = ih `div` 2

state :: Int -> ScreenState
state headerHeight = ScreenState headerHeight 0 Desktop

spec :: Spec
spec = do
  describe "QuickCreate" $ do

    describe "mkQuickCreateSelectionTop" $ do
      it "calculates the selection top in the main section for a selection at the top of the screen" $ do
        mkQuickCreateSelectionTop (rangeTopFor mh 0) (state 0) `shouldBe` 0

      it "selection top is further down when the selection is lower" $ do
        mkQuickCreateSelectionTop (rangeTopFor (mh+200) 0) (state 0) `shouldBe` 200

      it "selection top is further down when the page is scrolled" $ do
        mkQuickCreateSelectionTop (rangeTopFor (mh+200) 100) (state 0) `shouldBe` 300

      it "selection top is higher up when the header is higher" $ do
        mkQuickCreateSelectionTop (rangeTopFor (mh+200) 0) (state 100) `shouldBe` 100


    describe "mkQuickCreateSelectionPos" $ do
      it "calculates the position of the button relative to the top of the selection" $ do
        mkQuickCreateSelectionPos (rangePosFor 0 100) `shouldBe` 50 - ih2

      it "position is the same even when the selection is shifted" $ do
        mkQuickCreateSelectionPos (rangePosFor 1000 1100) `shouldBe` 50 - ih2

      it "positions the button at the center when the device offset is near the top and the selection is 200 or less" $ do
        mkQuickCreateSelectionPos (rangePosFor 100 300) `shouldBe` 100 - ih2

      it "positions the button at the top when the device offset is near the top and the selection is larger than 200" $ do
        mkQuickCreateSelectionPos (rangeFor 120 100 301 0) `shouldBe` 0

      it "positions the button at the bottom when the device offset is near the bottom and the selection is larger than 200" $ do
        mkQuickCreateSelectionPos (rangeFor 280 100 301 0) `shouldBe` 201 - ih


    describe "mkQuickCreateOffset" $ do
      it "calculates the offset for one line at the top of the screen which is minus half the icon height" $ do
         mkQuickCreateOffset (rangeFor 0 mh mh 0) (state 0) `shouldBe` (-22)

      it "hits the upper edge if the box is iconHeight high" $ do
         mkQuickCreateOffset (rangeFor 0 mh (mh+ih) 0) (state 0) `shouldBe` 0

      it "selects the center of the box if the box is not too high" $ do
         mkQuickCreateOffset (rangeFor 0 mh (100+mh+ih) 0) (state 0) `shouldBe` 50

      it "moves the offset down if the selection is further down" $ do
         mkQuickCreateOffset (rangeFor 0 (200+mh) (200+mh+ih) 0) (state 0) `shouldBe` 200

      it "moves the offset further down if the selection is further down and the box is not too high" $ do
         mkQuickCreateOffset (rangeFor 0 (200+mh) (300+mh+ih) 0) (state 0) `shouldBe` 250

      it "subtracts the height of the header from the offset" $ do
         mkQuickCreateOffset (rangeFor 0 (200+mh) (300+mh+ih) 0) (state 100) `shouldBe` 150

      it "adds the amount that was scrolled to the offset" $ do
         mkQuickCreateOffset (rangeFor 0 (200+mh) (300+mh+ih) 100) (state 0) `shouldBe` 350

      it "positions the top closer to the bottom if a large selection ended there" $ do
         mkQuickCreateOffset (rangeFor 1300 (200+mh) (1200+mh+ih) 0) (state 0) `shouldBe` 1200

      it "positions the top closer to the top if a large selection ended there" $ do
         mkQuickCreateOffset (rangeFor 300 (200+mh) (1200+mh+ih) 0) (state 0) `shouldBe` 200
