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

module Refine.Frontend.Contributions.QuickCreateSpec where

import Test.Hspec

import qualified Refine.Frontend.Contributions.Types as RS
import           Refine.Frontend.Contributions.QuickCreate
import qualified Refine.Frontend.Screen.Types as RS


rangeTopFor :: Int -> Int -> RS.Range
rangeTopFor topOffset scrollOffset =
  RS.Range Nothing Nothing (RS.OffsetFromViewportTop topOffset) 0 (RS.ScrollOffsetOfViewport scrollOffset)

rangePosFor :: Int -> Int -> RS.Range
rangePosFor topOffset bottom =
  RS.Range Nothing Nothing (RS.OffsetFromViewportTop topOffset) bottom (RS.ScrollOffsetOfViewport 0)

rangeFor :: Int -> Int -> Int -> RS.Range
rangeFor topOffset bottom scrollOffset =
  RS.Range Nothing Nothing (RS.OffsetFromViewportTop topOffset) bottom (RS.ScrollOffsetOfViewport scrollOffset)

mh :: Int
mh = 80 -- menu height
ih :: Int
ih = 44 -- icon height
ih2 :: Int
ih2 = ih `div` 2


spec :: Spec
spec = do
  describe "QuickCreate" $ do

    describe "quickCreateSelectionTop" $ do
      it "calculates the selection top in the main section for a selection at the top of the screen" $ do
        quickCreateSelectionTop (rangeTopFor mh 0) (RS.ScreenState 0 RS.Desktop) `shouldBe` 0

      it "selection top is further down when the selection is lower" $ do
        quickCreateSelectionTop (rangeTopFor (mh+200) 0) (RS.ScreenState 0 RS.Desktop) `shouldBe` 200

      it "selection top is further down when the page is scrolled" $ do
        quickCreateSelectionTop (rangeTopFor (mh+200) 100) (RS.ScreenState 0 RS.Desktop) `shouldBe` 300

      it "selection top is higher up when the header is higher" $ do
        quickCreateSelectionTop (rangeTopFor (mh+200) 0) (RS.ScreenState 100 RS.Desktop) `shouldBe` 100


    describe "quickCreateSelectionPos" $ do
      it "calculates the position of the button relative to the top of the selection" $ do
        quickCreateSelectionPos (rangePosFor 0 100) 0 `shouldBe` 50 - ih2

      it "position is the same even when the selection is shifted" $ do
        quickCreateSelectionPos (rangePosFor 1000 1100) 0 `shouldBe` 50 - ih2

      it "positions the button at the center when the device offset is near the top and the selection is 200 or less" $ do
        quickCreateSelectionPos (rangePosFor 100 300) 0 `shouldBe` 100 - ih2

      it "positions the button at the top when the device offset is near the top and the selection is larger than 200" $ do
        quickCreateSelectionPos (rangePosFor 100 301) 120 `shouldBe` 0

      it "positions the button at the bottom when the device offset is near the bottom and the selection is larger than 200" $ do
        quickCreateSelectionPos (rangePosFor 100 301) 280 `shouldBe` 201 - ih


    describe "quickCreateOffset" $ do
      it "calculates the offset for one line at the top of the screen which is minus half the icon height" $ do
         quickCreateOffset (rangeFor mh mh 0) 0 (RS.ScreenState 0 RS.Desktop) `shouldBe` (-22)

      it "hits the upper edge if the box is iconHeight high" $ do
         quickCreateOffset (rangeFor mh (mh+ih) 0) 0 (RS.ScreenState 0 RS.Desktop) `shouldBe` 0

      it "selects the center of the box if the box is not too high" $ do
         quickCreateOffset (rangeFor mh (100+mh+ih) 0) 0 (RS.ScreenState 0 RS.Desktop) `shouldBe` 50

      it "moves the offset down if the selection is further down" $ do
         quickCreateOffset (rangeFor (200+mh) (200+mh+ih) 0) 0 (RS.ScreenState 0 RS.Desktop) `shouldBe` 200

      it "moves the offset further down if the selection is further down and the box is not too high" $ do
         quickCreateOffset (rangeFor (200+mh) (300+mh+ih) 0) 0 (RS.ScreenState 0 RS.Desktop) `shouldBe` 250

      it "subtracts the height of the header from the offset" $ do
         quickCreateOffset (rangeFor (200+mh) (300+mh+ih) 0) 0 (RS.ScreenState 100 RS.Desktop) `shouldBe` 150

      it "adds the amount that was scrolled to the offset" $ do
         quickCreateOffset (rangeFor (200+mh) (300+mh+ih) 100) 0 (RS.ScreenState 0 RS.Desktop) `shouldBe` 350

      it "positions the top closer to the bottom if a large selection ended there" $ do
         quickCreateOffset (rangeFor (200+mh) (1200+mh+ih) 0) 1300 (RS.ScreenState 0 RS.Desktop) `shouldBe` 1200

      it "positions the top closer to the top if a large selection ended there" $ do
         quickCreateOffset (rangeFor (200+mh) (1200+mh+ih) 0) 300 (RS.ScreenState 0 RS.Desktop) `shouldBe` 200
