{-# LANGUAGE CPP #-}
#include "language_common.hs"

{-# OPTIONS_GHC -fno-warn-orphans             #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- pattern completeness checker has problems with pattern synonyms

module Refine.Common.Test.Arbitrary where

#include "import_common.hs"

import           Control.Arrow (first, second)
import           Control.DeepSeq
import           Data.Function (on)
import           Data.List (groupBy)
import           Generics.SOP
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import "quickcheck-instances"
                 Test.QuickCheck.Instances ()

import Refine.Common.Error
import Refine.Common.OT as OT
import Refine.Common.Types
import Refine.Common.Types.Core (OTDoc)
import Refine.Common.Route


-- * generics

garbitrary' :: forall a. (Int -> Int) -> (SOP.Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary' scaling = SOP.to <$> (hsequence =<< elements subs)
  where
    subs :: [SOP Gen (Code a)]
    subs = apInjs_POP (hcpure (Proxy @Arbitrary) (scale scaling arbitrary))

garbitrary :: forall a. (SOP.Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = garbitrary' id

gshrink :: forall a . (SOP.Generic a, All2 Arbitrary (Code a)) => a -> [a]
gshrink = List.map SOP.to . shrinkSOP . from
  where
    shrinkSOP :: All2 Arbitrary xss => SOP I xss -> [SOP I xss]
    shrinkSOP (SOP nsp) = SOP <$> shrinkNS nsp

    shrinkNS :: All2 Arbitrary xss => NS (NP I) xss -> [NS (NP I) xss]
    shrinkNS (Z Nil) = []
    shrinkNS (Z np)  = Z <$> (hsequence . hap (hcpure (Proxy @Arbitrary) (mkFn shrink))) np
    shrinkNS (S ns)  = S <$> shrinkNS ns

    mkFn f = SOP.Fn (f . unI)


-- * generic instances

instance Arbitrary a => Arbitrary (CreateGroup_ a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (EditSource a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (MetaID a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (NoJSONRep a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (OT.Atom a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (OT.NonEditable a) where arbitrary = garbitrary
instance Arbitrary Abstract where arbitrary = garbitrary
instance Arbitrary ApiErrorCreateUser where arbitrary = garbitrary
instance Arbitrary ApiErrorDB where arbitrary = garbitrary
instance Arbitrary ApiError where arbitrary = garbitrary
instance Arbitrary BlockIndex where arbitrary = garbitrary
instance Arbitrary CacheKey where arbitrary = garbitrary
instance Arbitrary CreateStatement where arbitrary = garbitrary
instance Arbitrary CreateUser where arbitrary = garbitrary
instance Arbitrary CreateVDoc where arbitrary = garbitrary
instance Arbitrary Discussion where arbitrary = garbitrary
instance Arbitrary EditKind where arbitrary = garbitrary
instance Arbitrary EditStats where arbitrary = garbitrary
instance Arbitrary Group where arbitrary = garbitrary
instance Arbitrary ImageInline where arbitrary = garbitrary
instance Arbitrary MetaInfo where arbitrary = garbitrary
instance Arbitrary Refine.Common.Types.Edit where arbitrary = garbitrary
instance Arbitrary RouteParseError where arbitrary = garbitrary
instance Arbitrary Statement where arbitrary = garbitrary
instance Arbitrary Timespan where arbitrary = garbitrary
instance Arbitrary Timestamp where arbitrary = garbitrary
instance Arbitrary Title where arbitrary = garbitrary
instance Arbitrary UpdateVDoc where arbitrary = garbitrary
instance Arbitrary UserInfo where arbitrary = garbitrary
instance Arbitrary User where arbitrary = garbitrary
instance Arbitrary VDoc where arbitrary = garbitrary
instance Arbitrary Vote where arbitrary = garbitrary

instance (Editable a, Arbitrary a) => Arbitrary (OT.EEdit a) where
  arbitrary = do
    old <- arbitrary
    new <- arbitrary
    let Right eedit = diff old new
    elements eedit


-- * more specific instances

instance Arbitrary L10 where
  arbitrary = L10 <$> scale (`div` 3) arbitrary <*> arbitrary

instance Arbitrary I18n.Locale where
  arbitrary = I18n.Locale . cs . show <$> elements [(1 :: Int)..10]

instance Arbitrary I18n.Msgid where
  arbitrary = I18n.Msgid . cs . show <$> elements [(1 :: Int)..10]


instance Arbitrary (ID a) where
  arbitrary = ID <$> arbitrary

instance Arbitrary ContributionID where
  arbitrary = arbitraryContribIDConstructor <*> arbitrary

arbitraryContribIDConstructor :: Gen (Int -> ContributionID)
arbitraryContribIDConstructor = elements
  [ ContribIDDiscussion True  . ID . fromIntegral . abs
  , ContribIDDiscussion False . ID . fromIntegral . abs
  , ContribIDEdit             . ID . fromIntegral . abs
  ]

instance Arbitrary MarkID where
  arbitrary = oneof
    [ pure MarkCurrentSelection
    , MarkContribution <$> arbitrary <*> pure 0
    ]

maxListOf :: Int -> Gen a -> Gen [a]
maxListOf n g = List.take n <$> listOf g

instance Arbitrary GroupRole where
  arbitrary = elements [minBound..]

instance Arbitrary GlobalRole where
  arbitrary = elements [minBound..]


-- * draft.js

-- (the upper bound of 36 for depth is arbitrarily introduced here.  don't know about draft.)
instance Arbitrary BlockDepth where
  arbitrary = BlockDepth <$> choose (0, 36)

instance Arbitrary RawContent where
  arbitrary = (rawContentBlocks %~ initBlockKeys) . docToRawContent . NEL.fromList <$> listOf1 (scale (`div` 3) arbitrary)
       -- alternative implementation: @initBlockKeys . sanitizeRawContent . mkRawContent <$> arbitrary@
  shrink    = canonicalizeRawContent . sanitizeRawContent <$$> gshrink

legalChar :: Char -> Bool
legalChar = (`notElem` ['\\'])  -- (occurrances of '\\' shift ranges around in the test suite.)
  -- more conservatively, we could test against this: (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

-- | These are the sanity conditions imposed on 'ContentState' by the draft library.  Everything
-- that does not meet these conditions will be silently removed from the input.
--
-- NOTES: (1) some restrictions may be encoded in the arbitrary methods instead of here, but not in
-- the shrink methods, so it is still useful to have 'sanitizeRawContent'; (2) this function is not
-- optimized for run-time, but for readability.  if you want to use it in production, check if you
-- want to improve on that first.
sanitizeRawContent :: RawContent -> RawContent
sanitizeRawContent = deleteDanglingEntityRefs
                   . deleteDanglingEntities
                   . deleteOverlappingEntities
                   . mergeOverlappingStyles
                   . deleteBadRanges
                   . boundDepth
                   . removeIllegalChars
  where
    removeIllegalChars :: RawContent -> RawContent
    removeIllegalChars (RawContent bs es) = RawContent ((blockText %~ ST.filter legalChar) <$> bs) es

    boundDepth (RawContent bs es) = RawContent ((blockDepth %~ (min 36 . max 0)) <$> bs) es
      -- (ok, the upper bound of 36 for depth is arbitrarily introduced here.  don't know about draft.)

    deleteBadRanges (RawContent bs es) = RawContent (sieve <$> bs) es
      where
        sieve :: Block EntityKey BlockKey -> Block EntityKey BlockKey
        sieve b = b & blockEntityRanges %~ filter (good b . snd)
                    & blockStyles       %~ filter (good b . fst)

        good :: Block EntityKey BlockKey -> EntityRange -> Bool
        good b (EntityRange offset len) = let ml = ST.length (b ^. blockText) in and
          [ offset >= 0
          , offset < ml
          , len > 0  -- (ranges must be non-empty, i think)
          , len < ml - offset
          ]

    deleteDanglingEntityRefs (RawContent bs es) = RawContent (sieve <$> bs) es
      where
        sieve :: Block EntityKey BlockKey -> Block EntityKey BlockKey
        sieve b = b & blockEntityRanges %~ filter (\(EntityKey k, _) -> k `IntMap.member` es)

    deleteDanglingEntities (RawContent bs es) = entityKeys `deepseq` RawContent bs (go es)
      where
        go :: IntMap a -> IntMap a
        go = IntMap.filterWithKey (\k _ -> k `Set.member` entityKeys)

        entityKeys :: Set Int
        entityKeys = Set.fromList $ coerce . fst <$> mconcat (view blockEntityRanges <$> NEL.toList bs)

    deleteOverlappingEntities :: RawContent -> RawContent
    deleteOverlappingEntities  (RawContent bs es) = RawContent ((blockEntityRanges %~ goEntities) <$> bs) es
      where
        goEntities :: (Eq a, Ord a) => [(a, EntityRange)] -> [(a, EntityRange)]
        goEntities = go _2 _1

        -- TUNING: we could try folding the groups into a payload-keyed map.  that would avoid the
        -- multiple sort steps, but i'm skeptical about the impact on overall test suite run times
        -- changing the entityKeys type in deleteDanglingEntities above from list to set didn't have
        -- any effect that could be measured by just running the test suite and looking at the
        -- clock.  ~fisx
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
            goGroup (x@(view range -> EntityRange o l) : x'@(view range -> EntityRange o' l') : xs)
              | o + l == o' = goGroup ((x & range .~ EntityRange o (l + l')) : xs)
              | o + l >  o' = goGroup (x' : xs)
              | otherwise   = x : goGroup (x' : xs)

    -- if two style ranges overlap for the same (Eq) thing, they will merge into one range.  note
    -- that this code is implemented to also work for entities, but entities are not allowed to
    -- overlap, and touching entities are not allowed to merge.
    mergeOverlappingStyles :: RawContent -> RawContent
    mergeOverlappingStyles (RawContent bs es) = RawContent ((blockStyles %~ goStyles) <$> bs) es
      where
        goStyles :: (Eq a, Ord a) => [(EntityRange, a)] -> [(EntityRange, a)]
        goStyles = go _1 _2

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
            goGroup (x@(view range -> EntityRange o l) : x'@(view range -> EntityRange o' l') : xs)
              = if o + l >= o'
                  then goGroup ((x & range .~ merge_ (o, l) (o', l')) : xs)
                  else x : goGroup (x' : xs)

            merge_ (o, l) (o', l') = EntityRange o'' l''
              where
                o'' = min o o'
                l'' = max (o + l) (o' + l') - o''

instance (SOP.Generic a, Arbitrary a, Ord a, SOP.Generic b, Arbitrary b) => Arbitrary (Block a b) where
  arbitrary = do
    b <- garbitrary
    let ml = ST.length (b ^. blockText)
        fixrange (EntityRange off len) = EntityRange off' len'
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

instance (Arbitrary a, Arbitrary b) => Arbitrary (GPosition a b) where
  arbitrary = Position <$> arbitrary <*> arbitrary
  shrink    = gshrink

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ Range a b
  shrink = reorder <$$> gshrink
    where
      reorder (Range a b) = Range a b -- note that Range is a smart constructor

instance (Arbitrary a, Ord a) => Arbitrary (Selection a) where
  arbitrary = garbitrary
  shrink    = gshrink

instance Arbitrary SelectionState where
  arbitrary = garbitrary
  shrink    = gshrink

data RawContentWithSelections = RawContentWithSelections RawContent [Selection Position]
  deriving (Eq, Show)

instance Arbitrary RawContentWithSelections where
  arbitrary = do
    rc <- arbitrary
    ss <- replicateM 11 $ arbitrarySoundSelectionState rc
    pure $ RawContentWithSelections rc ss

arbitrarySoundPosition :: RawContent -> Gen Position
arbitrarySoundPosition (RawContent bs _) = do
    i <- choose (0, NEL.length bs - 1)
    let b = bs NEL.!! i
        blockkey = b ^. blockKey
    offset <- choose (0, max 0 (ST.length (b ^. blockText) - 1))
    pure $ Position (BlockIndex i blockkey) offset

-- the range may be empty
arbitrarySoundRange :: RawContent -> Gen (Range Position)
arbitrarySoundRange rc = Range <$> arbitrarySoundPosition rc <*> arbitrarySoundPosition rc

arbitrarySoundSelectionState :: RawContent -> Gen (Selection Position)
arbitrarySoundSelectionState rc = Selection <$> arbitrary <*> arbitrarySoundRange rc

instance SOP.Generic NonEmptyST
instance Arbitrary NonEmptyST where
  -- HACK: legalChar is needed for the Arbitrary instance of OTDoc
  arbitrary = NonEmptyST . ST.pack <$> scale (`div` 2) (listOf1 $ arbitrary `suchThat` legalChar)

instance (Arbitrary a, Arbitrary b, Eq a, Splitable b) => Arbitrary (Segments a b) where
  arbitrary = Segments . map (\xs@(x:_) -> (fst x, foldr1 joinItems $ snd <$> xs)) . groupBy ((==) `on` fst)
        <$> arbitrary

instance (Ord a, EqProp a) => EqProp (Range a) where Range a b =-= Range c d = a =-= c .&. b =-= d

instance (Arbitrary a, Ord a) => Arbitrary (Ranges a) where
  arbitrary = sized $ \case
    0 -> pure mempty
    _ -> oneof
            [ pure mempty
            , do
                r <- arbitrary
                rs <- scale (`div` 2) arbitrary
                pure $ r <> rs
            ]

instance (Ord a, EqProp a) => EqProp (Ranges a) where (=-=) = (=-=) `on` unRanges


-- | Allows to insert a 'TestBatch' into a Spec.
--
-- hilariously, there is a package just for these two lines:
-- https://hackage.haskell.org/package/hspec-checkers-0.1.0.2
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " <> batchName) $ mapM_ (uncurry it) tests


instance Arbitrary ServerCache where
  arbitrary = genServerCache True
  shrink (ServerCache a b c d e f g) = ServerCache
    <$> shrinkDataMap a
    <*> shrinkDataMap b
    <*> shrinkDataMap c
    <*> shrinkDataMap d
    <*> shrinkDataMap e
    <*> shrink f
    <*> shrink g

genServerCache :: Bool{- shallow -} -> Gen ServerCache
genServerCache True = ServerCache
  <$> (Map.fromList . fmap (, undefined) <$> arbitrary)
  <*> (Map.fromList . fmap (, undefined) <$> arbitrary)
  <*> (Map.fromList . fmap (, undefined) <$> arbitrary)
  <*> (Map.fromList . fmap (, undefined) <$> arbitrary)
  <*> (Map.fromList . fmap (, undefined) <$> arbitrary)
  <*> arbitrary
  <*> arbitrary

genServerCache False = ServerCache
  <$> genDataMap arbitrary arbitrary
  <*> genDataMap arbitrary arbitrary
  <*> genDataMap arbitrary arbitrary
  <*> genDataMap arbitrary arbitrary
  <*> genDataMap arbitrary arbitrary
  <*> arbitrary
  <*> arbitrary

genDataMap :: (Ord a) => Gen a -> Gen b -> Gen (Map a b)
genDataMap k v = do
  len <- choose (0, 5)
  ks <- vectorOf len k
  vs <- vectorOf len v
  pure . Map.fromList $ zip ks vs

shrinkDataMap :: forall a b. (Ord a, Arbitrary a) => Map a b -> [Map a b]
shrinkDataMap m = chop <$> shrink (Map.keys m)
  where
    chop :: [a] -> Map a b
    chop = Map.fromList . fmap (\k -> (k, (\(Just v) -> v) $ Map.lookup k m))

-- | Collect all cache keys that occur in server cache as a set.
allKeysInServerCache :: ServerCache -> [CacheKey]
allKeysInServerCache = nub . sort . mconcat . fingerprintServerCache

-- | Collect all cache keys that occur in server cache in a list of cache.  (Output is different if
-- cache key is in a different part of the cache in the input.)
fingerprintServerCache :: ServerCache -> [[CacheKey]]
fingerprintServerCache sc =
  [ sort . fmap CacheKeyVDoc . Map.keys $ sc ^. scVDocs
  , sort . fmap CacheKeyEdit . Map.keys $ sc ^. scEdits
  , sort . fmap CacheKeyDiscussion . Map.keys $ sc ^. scDiscussions
  , sort . fmap CacheKeyUser . Map.keys $ sc ^. scUsers
  , sort . fmap CacheKeyGroup . Map.keys $ sc ^. scGroups
  , sort . fmap CacheKeyGroup . maybe [] Set.toList $ sc ^. scGroupIds
  , sort . fmap CacheKeyUser . maybe [] Set.toList $ sc ^. scUserIds
  ]

arbitrarySubset :: (Ord a) => Set a -> Gen (Set a)
arbitrarySubset oldset@(Set.size -> oldsize) = do
  newsize <- choose (0, oldsize)
  newixs :: [Int] <- take newsize <$> permutationGen oldsize
  pure . Set.fromList $ (Set.toList oldset !!) <$> newixs

permutationGen :: Int -> Gen [Int]
permutationGen 0 = pure []
permutationGen i = (:) <$> choose (0, i-1) <*> permutationGen (i-1)


data ServerCacheWithKeys = ServerCacheWithKeys ServerCache (Set CacheKey)

instance Eq ServerCacheWithKeys where
  ServerCacheWithKeys sc keys == ServerCacheWithKeys sc' keys' =
    fingerprintServerCache sc == fingerprintServerCache sc' && keys == keys'

instance Show ServerCacheWithKeys where
  show (ServerCacheWithKeys sc keys) = show (fingerprintServerCache sc, keys)

instance Arbitrary ServerCacheWithKeys where
  arbitrary = do
    sc <- arbitrary
    ServerCacheWithKeys sc <$> arbitrarySubset (Set.fromList $ [CacheKeyGroupIds, CacheKeyUserIds] <> allKeysInServerCache sc)
  shrink (ServerCacheWithKeys sc keys) = pruneKeys <$> shrink sc
    where
      pruneKeys sc' = ServerCacheWithKeys sc' (keys `Set.intersection` Set.fromList (allKeysInServerCache sc'))
