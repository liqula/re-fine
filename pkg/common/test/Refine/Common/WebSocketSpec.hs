{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.WebSocketSpec where
#include "import_common.hs"

import Test.Hspec
import Test.QuickCheck

import Refine.Common.Types
import Refine.Common.WebSocket
import Refine.Common.Test.Arbitrary


spec :: Spec
spec = do
  let cache groups = mempty
        & scGroups   .~ Map.fromList ((, undefined) <$> groups)
        & scGroupIds .~ Just (Set.fromList groups)
        & scUserIds  .~ Just (Set.fromList [0, 1, 2])

  describe "restrictCache" $ do
    context "for cache item key" $ do
      it "removes keys from item maps" $ do
        let have   = restrictCache (Set.fromList [CacheKeyGroup 0]) (cache [0, 1])
            should = cache [0]
            focus  = scGroups . to Map.keys
        have ^. focus `shouldBe` should ^. focus

      it "empties item maps for which no keys are mentioned" $ do
        let have   = restrictCache (Set.fromList [CacheKeyVDoc 0]) (cache [0, 1])
            should = cache []
            focus  = scGroups . to Map.keys
        have ^. focus `shouldBe` should ^. focus

      it "sets group key set to Nothing if group key gets invalidated" $ do
        let have = restrictCache (Set.fromList [CacheKeyGroup 0]) (cache [0, 1])
        have ^. scGroupIds `shouldBe` Nothing

      it "sets other key sets to Nothing if group key gets invalidated" $ do
        let have = restrictCache (Set.fromList [CacheKeyGroup 0]) (cache [0, 1])
        have ^. scUserIds `shouldBe` Nothing

    context "for CacheKeyGroupIds" $ do
      it "removes all group items from map" $ do
        let have = restrictCache (Set.fromList [CacheKeyGroupIds]) (cache [0, 1])
        have ^. (scGroups . to Map.keys) `shouldBe` []

      it "leaves group key set intact" $ do
        let have = restrictCache (Set.fromList [CacheKeyGroupIds]) (cache [0, 1])
        have ^. scGroupIds `shouldBe` Just (Set.fromList [0, 1])

  describe "invalidateCache" $ do
    it "is the inverse of 'restrictCache'." . property $ \(ServerCacheWithKeys sc keys) -> do
      let r = restrictCache keys sc
          i = invalidateCache keys sc
          k = fingerprintServerCache

      ('1', k sc)        `shouldBe` ('1', k (r <> i))
      ('2', zipWith (\\) (k sc) (k r)) `shouldBe` ('2', k i)
      ('3', zipWith (\\) (k sc) (k i)) `shouldBe` ('3', k r)

    -- (not sure the remaining 'invalidateCache' tests contribute anything meaningful.  remove if
    -- they are in the way!)

    context "for cache item key" $ do
      it "removes all other keys from item maps" $ do
        let have   = invalidateCache (Set.fromList [CacheKeyGroup 0]) (cache [0, 1])
            should = cache [1]
            focus  = scGroups . to Map.keys
        have ^. focus `shouldBe` should ^. focus

      it "leaves item maps for which no keys are mentioned intact" $ do
        let have   = invalidateCache (Set.fromList [CacheKeyVDoc 0]) (cache [0, 1])
            should = cache [0, 1]
            focus  = scGroups . to Map.keys
        have ^. focus `shouldBe` should ^. focus

      it "leaves group key set intact if group key gets invalidated" $ do
        let have = invalidateCache (Set.fromList [CacheKeyGroup 0]) (cache [0, 1])
        have ^. scGroupIds `shouldBe` Just (Set.fromList [0, 1])

      it "leaves other key sets intact if group key gets invalidated" $ do
        let have = invalidateCache (Set.fromList [CacheKeyGroup 0]) (cache [0, 1])
        have ^. scUserIds `shouldBe` Just (Set.fromList [0, 1, 2])

    context "for CacheKeyGroupIds" $ do
      it "leaves all group items in map intact" $ do
        let have = invalidateCache (Set.fromList [CacheKeyGroupIds]) (cache [0, 1])
        have ^. (scGroups . to Map.keys) `shouldBe` [0, 1]

      it "sets group key set to Nothing" $ do
        let have = invalidateCache (Set.fromList [CacheKeyGroupIds]) (cache [0, 1])
        have ^. scGroupIds `shouldBe` Nothing

  describe "intersectCacheWithKeySet" $ do
    it "keeps item key if it is in cache" $ do
      intersectCacheWithKeySet (cache [1, 2]) (Set.fromList [CacheKeyGroup 1])
        `shouldBe` Set.fromList [CacheKeyGroup 1]

    it "drops item key if it is *not* in cache" $ do
      intersectCacheWithKeySet (cache [1, 2]) (Set.fromList [CacheKeyGroup 3])
        `shouldNotBe` Set.fromList [CacheKeyGroup 3]

    it "drops list key if list is Nothing" $ do
      intersectCacheWithKeySet mempty (Set.fromList [CacheKeyGroupIds])
        `shouldBe` Set.fromList []

    it "keeps list key if list is Just" $ do
      Set.toList (intersectCacheWithKeySet (mempty & scGroupIds .~ Just mempty) (Set.fromList [CacheKeyGroupIds]))
        `shouldContain` [CacheKeyGroupIds]
      Set.toList (intersectCacheWithKeySet (mempty & scUserIds .~ Just mempty) (Set.fromList [CacheKeyUserIds]))
        `shouldContain` [CacheKeyUserIds]

    it "returns a (possibly non-proper) subset of its input" . property $ \(ServerCacheWithKeys sc keys) -> do
      let keys' = keys <> Set.fromList (f <$> Set.toList keys)  -- (Data.Set has no Functor instance...)
            where
              f (CacheKeyVDoc       (ID i)) = CacheKeyVDoc       (ID (i * 3))
              f (CacheKeyEdit       (ID i)) = CacheKeyEdit       (ID (i * 3))
              f (CacheKeyDiscussion (ID i)) = CacheKeyDiscussion (ID (i * 3))
              f (CacheKeyUser       (ID i)) = CacheKeyUser       (ID (i * 3))
              f (CacheKeyGroup      (ID i)) = CacheKeyGroup      (ID (i * 3))
              f CacheKeyGroupIds            = CacheKeyGroupIds
              f CacheKeyUserIds             = CacheKeyUserIds

      intersectCacheWithKeySet sc keys `shouldSatisfy` (`Set.isSubsetOf` keys)
      intersectCacheWithKeySet sc keys' `shouldSatisfy` (`Set.isSubsetOf` keys')
