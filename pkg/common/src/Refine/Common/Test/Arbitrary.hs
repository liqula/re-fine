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

import           Control.Arrow (first, second)
import           Control.Lens (Lens', (^.), (^?!), (.~), (%~), (&), _Just, view, _1, _2)
import           Control.Monad.State
import           Data.Function (on)
import qualified Data.IntMap as IntMap
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
  arbitrary = initBlockKeys . sanitizeRawContent . mkRawContent <$> arbitrary
  shrink    = fmap sanitizeRawContent . filter ((/= []) . view rawContentBlocks) <$> gshrink

initBlockKeys :: RawContent -> RawContent
initBlockKeys = rawContentBlocks %~ zipWith (\k -> blockKey .~ (Just . BlockKey . cs . show $ k)) [(0 :: Int)..]

-- | These are the sanity conditions imposed on 'ContentState' by the draft library.  Everything
-- that does not meet these conditions will be silently removed from the input.
--
-- NOTES: (1) some restrictions may be encoded in the arbitrary methods instead of here, but not in
-- the shrink methods, so it is still useful to have 'sanitizeRawContent'; (2) this function is not
-- optimized for run-time, but for readability.  if you want to use it in production, check if you
-- want to improve on that first.
sanitizeRawContent :: RawContent -> RawContent
sanitizeRawContent = mergeOverlaps . deleteDanglingEntityRefs . deleteDanglingEntities . deleteBadRanges . boundDepth . removeIllegalChars
  where
    removeIllegalChars (RawContent bs es) = RawContent ((blockText %~ ST.filter ok) <$> bs) es
      where
        ok :: Char -> Bool
        ok = (`notElem` ['\\'])  -- (occurrances of '\\' shift ranges around in the test suite.)
          -- more conservatively, we could test against this: (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

    boundDepth (RawContent bs es) = RawContent ((blockDepth %~ (min 36 . max 0)) <$> bs) es
      -- (ok, the upper bound of 36 for depth is arbitrarily introduced here.  don't know about draft.)

    deleteBadRanges (RawContent bs es) = RawContent (sieve <$> bs) es
      where
        sieve :: Block EntityKey -> Block EntityKey
        sieve b = b & blockEntityRanges %~ filter (good b . snd)
                    & blockStyles       %~ filter (good b . fst)

        good :: Block EntityKey -> (Int, Int) -> Bool
        good b (offset, len) = let ml = ST.length (b ^. blockText) in and
          [ offset >= 0
          , offset < ml
          , len > 0  -- (ranges must be non-empty, i think)
          , len < ml - offset
          ]

    deleteDanglingEntityRefs (RawContent bs es) = RawContent (sieve <$> bs) es
      where
        sieve :: Block EntityKey -> Block EntityKey
        sieve b = b & blockEntityRanges %~ filter (\(EntityKey k, _) -> k `elem` IntMap.keys es)

    deleteDanglingEntities (RawContent bs es) = RawContent bs $ IntMap.filterWithKey (\k _ -> EntityKey k `elem` entityKeys) es
      where
        entityKeys :: [EntityKey]
        entityKeys = fst <$> mconcat (view blockEntityRanges <$> bs)

    -- if two ranges overlap for the same (Eq) thing, they will merge into one range.  (FIXME: this
    -- works for styles, but entities must not overlap so we should filter them instead of merging
    -- them.)
    mergeOverlaps :: RawContent -> RawContent
    mergeOverlaps  (RawContent bs es) = RawContent ((blockStyles %~ goStyles) . (blockEntityRanges %~ goEntities) <$> bs) es
      where
        goStyles :: (Eq a, Ord a) => [(EntityRange, a)] -> [(EntityRange, a)]
        goStyles = go _1 _2

        goEntities :: (Eq a, Ord a) => [(a, EntityRange)] -> [(a, EntityRange)]
        goEntities = go _2 _1

        go :: forall a payload. (Eq payload, Ord payload) => Lens' a EntityRange -> Lens' a payload -> [a] -> [a]
        go range payload = sortR
                         . mconcat
                         . fmap (goGroup . sortR)
                         . sortGroupP
          where
            sortR      = List.sortBy (compare `on` view range)
            sortGroupP = List.groupBy ((==) `on` view payload)
                       . List.sortBy (compare `on` view payload)

            goGroup [] = []
            goGroup [x] = [x]
            goGroup (x@(view range -> (o, l)) : x'@(view range -> (o', l')) : xs)
              = if o + l >= o'
                  then goGroup ((x & range .~ merge (o, l) (o', l')) : xs)
                  else x : goGroup (x' : xs)

            merge (o, l) (o', l') = (o'', l'')
              where
                o'' = min o o'
                l'' = max (o + l) (o' + l') - o''


instance (Generic a, Arbitrary a) => Arbitrary (Block a) where
  arbitrary = do
    b <- garbitrary
    let ml = ST.length (b ^. blockText)
        fixrange (off, len) = (off', len')
          where
            off' = min (abs off) (abs (ml - 1))
            len' = min (abs len) (abs (ml - off'))
    pure $ case ml of
      0 -> b & blockEntityRanges .~ []
             & blockStyles       .~ []
      _ -> b & blockEntityRanges %~ fmap (second fixrange)
             & blockStyles       %~ fmap (first fixrange)

  shrink = gshrink

instance Arbitrary BlockKey where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary EntityKey where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary Entity where
  arbitrary = pure $ EntityLink "http://www.example.com"  -- invalid URLs are sanitized by draft.
  shrink _  = []

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
    rc <- arbitrary
    ss <- replicateM 11 $ arbitrarySoundSelectionState rc
    pure $ RawContentWithSelections rc ss

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

arbitrarySoundSelectionState :: RawContent -> Gen SelectionState
arbitrarySoundSelectionState (RawContent bs _) = do
  let arbpoint = do
        i <- choose (0, length bs - 1)
        let b = bs !! i
            blockkey = b ^?! blockKey . _Just
        offset <- choose (0, max 0 (ST.length (b ^. blockText) - 1))
        pure ( (i, offset)  -- this is used to tell which is start and which is end.
             , SelectionPoint blockkey offset
             )
  anchor <- arbpoint
  point  <- arbpoint
  isback <- arbitrary
  let [start, end] = snd <$> sort [anchor, point]
  pure $ SelectionState isback start end
