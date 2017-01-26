{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

module Refine.PreludeSpec where

import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Refine.Prelude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()


instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arbitrary

instance Arbitrary Timespan where
    arbitrary = do
        cns <- elements [TimespanUs, TimespanMs, TimespanSecs, TimespanMins, TimespanHours, TimespanDays]
        cns <$> arbitrary


spec :: Spec
spec = parallel $ do
  describe "time" $ do
    describe "Timestamp" $ do
        it "read and show are inverses" . property $
            \x -> read (show x) == (x :: Timestamp)
        it "readTimestamp and showTimestamp are inverses" . property $
            \x -> readTimestamp (showTimestamp x) == Just x
        it "readTimestamp should fail on noise" . property $
            isNothing . readTimestamp . (<> "noise") . showTimestamp

    describe "diffTimestamps(-), addTimespan(+)" $ do
        it "(y+x)-x = y" . property $
            \(x :: Timestamp) (y :: Timespan) ->
                timespanUs ((y `addTimespan` x) `diffTimestamps` x) `shouldBe` timespanUs y
        it "y-((y-x)+x) = 0" . property $
            \(x :: Timestamp) (y :: Timestamp) ->
                timespanUs (y `diffTimestamps` ((y `diffTimestamps` x) `addTimespan` x)) `shouldBe` 0

  describe "misc" $ do
    describe "commonPrefix" $ do
        it "empty lists" $
            commonPrefix ([] :: [Int]) [] `shouldBe` ([], [], [])
        it "empty first list" $
            commonPrefix [] [1 :: Int] `shouldBe` ([], [], [1])
        it "empty second list" $
            commonPrefix [1 :: Int] [] `shouldBe` ([], [1], [])
        it "non empty lists with common prefix" $
            commonPrefix [1 :: Int, 2, 3, 4, 5] [1, 2, 3, 5] `shouldBe` ([1, 2, 3], [4, 5], [5])
        it "non empty lists without common prefix" $
            commonPrefix [1 :: Int, 2, 3, 4, 5] [50, 2, 3, 5] `shouldBe` ([], [1, 2, 3, 4, 5], [50, 2, 3, 5])
        it "works with random additional prefix" . property $ \(ps :: [Int]) (as :: [Int]) (bs :: [Int]) ->
            collect (length ps) $
            let (ps0, as0, bs0) = commonPrefix as bs
                ps1 = ps <> ps0
                (ps2, as2, bs2) = commonPrefix (ps1 <> as0) (ps1 <> bs0)
            in ps1 == ps2 && as2 == as0 && bs2 == bs0
