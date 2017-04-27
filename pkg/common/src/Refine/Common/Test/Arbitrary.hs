{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test.Arbitrary where

import           Control.Lens ((^.), (^?!), (.~), (%~), _Just)
import           Control.Monad.State
import           Data.List (sort)
import qualified Data.List as List
import           Data.String.Conversions (cs)
import qualified Data.Text as ST
import qualified Data.Text.I18n as I18n
import           Generics.SOP
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import Refine.Common.Types
import Refine.Common.VDoc.Draft


instance Arbitrary L10 where
  arbitrary = L10 <$> scale (`div` 3) arbitrary <*> arbitrary

instance Arbitrary I18n.Locale where
  arbitrary = I18n.Locale . cs . show <$> elements [(1 :: Int)..10]

instance Arbitrary I18n.Msgid where
  arbitrary = I18n.Msgid . cs . show <$> elements [(1 :: Int)..10]


instance Arbitrary (ID a) where
  arbitrary = ID <$> arbitrary

instance Arbitrary DataUID where
  arbitrary = DataUID <$> arbitrary

instance Arbitrary ContributionID where
  arbitrary = oneof
    [ ContribIDNote <$> arbitrary
    , ContribIDQuestion <$> arbitrary
    , ContribIDDiscussion <$> arbitrary
    , ContribIDEdit <$> arbitrary
    , pure ContribIDHighlightMark
    ]

maxListOf :: Int -> Gen a -> Gen [a]
maxListOf n g = List.take n <$> listOf g

instance Arbitrary Role where
  arbitrary = elements [minBound..]


-- * draft.js

-- | copied from https://github.com/liqd/aula, file src/Arbitrary.hs
garbitrary' :: forall a. (Int -> Int) -> (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary' scaling = to <$> (hsequence =<< elements subs)
  where
    subs :: [SOP Gen (Code a)]
    subs = apInjs_POP (hcpure (Proxy @Arbitrary) (scale scaling arbitrary))

-- | copied from https://github.com/liqd/aula, file src/Arbitrary.hs
garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = garbitrary' (max 0 . subtract 10)

-- | copied from https://github.com/liqd/aula, file src/Arbitrary.hs
gshrink :: forall a . (Generic a, All2 Arbitrary (Code a)) => a -> [a]
gshrink = List.map to . shrinkSOP . from
  where
    shrinkSOP :: All2 Arbitrary xss => SOP I xss -> [SOP I xss]
    shrinkSOP (SOP nsp) = SOP <$> shrinkNS nsp

    shrinkNS :: All2 Arbitrary xss => NS (NP I) xss -> [NS (NP I) xss]
    shrinkNS (Z Nil) = []
    shrinkNS (Z np)  = Z <$> (hsequence . hap (hcpure (Proxy @Arbitrary) (mkFn shrink))) np
    shrinkNS (S ns)  = S <$> shrinkNS ns

    mkFn f = Fn (f . unI)


instance Arbitrary RawContent where
  arbitrary = garbitrary
  shrink    = gshrink

instance (Generic a, Arbitrary a) => Arbitrary (Block a) where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary BlockKey where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary EntityKey where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary Entity where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary Style where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary BlockType where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary SelectionState where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary SelectionPoint where
  arbitrary = garbitrary
  shrink    = gshrink

data RawContentWithSelections = RawContentWithSelections RawContent [SelectionState]
  deriving (Eq, Show)

instance Arbitrary RawContentWithSelections where
  arbitrary = do
    c  <- arbitraryNonNothingBlockKeys . makeNonEmpty <$> arbitrary
    ss <- replicateM 11 (arbitrarySoundSelectionState c)
    pure $ RawContentWithSelections c ss

makeNonEmpty :: RawContent -> RawContent
makeNonEmpty = rawContentBlocks %~ (b:)
  where
    b = Block
      { _blockText         = "wefwef"
      , _blockEntityRanges = []
      , _blockStyles       = []
      , _blockType         = minBound
      , _blockDepth        = 0
      , _blockKey          = Nothing
      }

arbitraryNonNothingBlockKeys :: RawContent -> RawContent
arbitraryNonNothingBlockKeys = rawContentBlocks %~ zipWith (\k -> blockKey .~ (Just . BlockKey . cs . show $ k)) [(0 :: Int)..]

arbitrarySoundSelectionState :: RawContent -> Gen SelectionState
arbitrarySoundSelectionState (RawContent bs _) = do
  let arbpoint = do
        i <- choose (0, length bs - 1)
        let b = bs !! i
            blockkey = b ^?! blockKey . _Just
        offset <- choose (0, ST.length (b ^. blockText) - 1)
        pure (i, SelectionPoint blockkey offset)
  anchor <- arbpoint
  point  <- arbpoint
  let [start, end] = snd <$> sort [anchor, point]
  pure $ SelectionState start end
