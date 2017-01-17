module Refine.Frontend.OverlaySpec where

import Test.Hspec

import qualified Refine.Frontend.RefineStore as RS

import           Refine.Frontend.Overlay

rangeFor :: Int -> Int -> Int -> RS.Range
rangeFor top bottom scrollOffset =
  RS.Range Nothing 0 Nothing 0 top bottom scrollOffset

mh :: Int
mh = 80 -- menu height
ih :: Int
ih = 44 -- icon height

spec :: Spec
spec = do
  describe "Overlay" $ do
    describe "quickCreateOffset" $ do
      it "calculates the offset for one line at the top of the screen" $ do
         quickCreateOffset (rangeFor mh mh 0) 0 0 `shouldBe` (-22)

      it "hits the upper edge if the box is iconHeight high" $ do
         quickCreateOffset (rangeFor mh (mh+ih) 0) 0 0 `shouldBe` 0

      it "selects the center of the box if the box is higher" $ do
         quickCreateOffset (rangeFor mh (200+mh+ih) 0) 0 0 `shouldBe` 100

      it "moves the offset down if the selection is further down" $ do
         quickCreateOffset (rangeFor (200+mh) (200+mh+ih) 0) 0 0 `shouldBe` 200

      it "moves the offset further down if the selection is further down and the box is higher" $ do
         quickCreateOffset (rangeFor (200+mh) (400+mh+ih) 0) 0 0 `shouldBe` 300

      it "subtracts the height of the header from the offset" $ do
         quickCreateOffset (rangeFor (200+mh) (400+mh+ih) 0) 0 100 `shouldBe` 200

      it "adds the amount that was scrolled to the offset" $ do
         quickCreateOffset (rangeFor (200+mh) (400+mh+ih) 100) 0 0 `shouldBe` 400
