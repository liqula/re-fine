{-# LANGUAGE NoImplicitPrelude          #-}
-- FUTUREWORK: release this file as a library

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.OTSpec where

import Refine.Common.Prelude

import qualified Data.Set as Set
import qualified Data.Text as ST
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Data.Sequence (Seq)
import           Test.Hspec
import           Test.QuickCheck

import Refine.Common.OT
import Refine.Common.Test.Arbitrary ()

---------------------------------------- quickcheck laws

-- | Auxiliary class needed for testing only
class (Editable d, Arbitrary d, Eq d, Show d, Show (EEdit d)) => GenEdit d where
    -- TUNING: return the edited structure too
    genEdit :: d -> Gen (Edit d)

runTest :: forall d. (Typeable d, GenEdit d) => [(String, d -> Gen Property)] -> Spec
runTest = runTest' $ RunTestConfig Nothing Nothing

data RunTestConfig = RunTestConfig
  { _runTestConfigRescale    :: Maybe Int
  , _runTestConfigMaxSuccess :: Maybe Int
  }
  deriving (Eq, Ord, Show)

runTest' :: forall d. (Typeable d, GenEdit d) => RunTestConfig -> [(String, d -> Gen Property)] -> Spec
runTest' (RunTestConfig mrescale mmaxsuccess) tests
    = describe ("Editable instance for " <> show (typeRep (Proxy :: Proxy d)))
    . forM_ tests $ \(name, test) -> itWith mmaxsuccess name
    $ property (scale (`div` fromMaybe 1 mrescale) <$> test)
  where
    itWith Nothing           name p = it name p
    itWith (Just maxsuccess) name p = it name $ do
      result <- quickCheckWithResult (stdArgs { maxSuccess = maxsuccess, chatty = False }) p
      result `shouldSatisfy` \case
        Test.QuickCheck.Success _ _ _ -> True
        _ -> False  -- arguably in this case we could have nicer failure reporting, but at least the
                    -- information is all there.

---------------------

fastTests :: GenEdit d => [(String, d -> Gen Property)]
fastTests =
    [ (,) "edit composition"    test_edit_composition
    , (,) "diamond"             test_diamond
    , (,) "diamond right join"  test_diamond_right_join
    , (,) "diamond left join"   test_diamond_left_join
    , (,) "inverse"             test_inverse
    , (,) "inverse of inverse"  test_inverse_inverse
    , (,) "inverted diamond"    test_inverse_diamond
    ]

hardTests :: GenEdit d => [(String, d -> Gen Property)]
hardTests =
    [ (,) "diff" test_diff
    , (,) "diff (2)" test_diff2
    ]

allTests :: GenEdit d => [(String, d -> Gen Property)]
allTests = fastTests <> hardTests

---------------------

-- | compare two edits by effect
equalEdit :: (Editable d, Eq d) => Edit d -> Edit d -> d -> Bool
equalEdit e0 e1 d = patch e0 d == patch e1 d

failPrint :: (Show a, Testable prop) => a -> prop -> Gen Property
failPrint a p = pure $ whenFail (print a) p

---------------------

test_edit_composition :: GenEdit d => d -> Gen Property
test_edit_composition d = do
    a <- genEdit d
    let d' = patch a d
    b <- genEdit d'
    failPrint (a, b) $ patch b d' == patch (a <> b) d

{- |
  a//\b  =  a/\\b
   \\/       \//
-}
test_diamond :: GenEdit d => d -> Gen Property
test_diamond d = do
    a <- genEdit d
    b <- genEdit d
    failPrint (a, b) $ equalEdit (a <> fst (merge d a b)) (b <> snd (merge d a b)) d

{- |
        a/\b  =  a/\b<>c
         \/\c     \ \
          \//      \//
-}
test_diamond_right_join :: GenEdit d => d -> Gen Property
test_diamond_right_join d = do
    (a, b, d', c) <- genPatchesForDiamondJoin d
    failPrint (a, b, c) $ equalEdit (snd $ merge d' (snd $ merge d a b) c) (snd $ merge d a (b <> c)) (patch c d')

-- | FIXME: I had to add this helper function only because hlint complained about duplicate code
genPatchesForDiamondJoin :: GenEdit d => d -> Gen (Edit d, Edit d, d, Edit d)
genPatchesForDiamondJoin d = do
    a <- genEdit d
    b <- genEdit d
    let d' = patch b d
    c <- genEdit d'
    pure (a, b, d', c)

{- | mirror of test_diamond_right_join

        b/\a  = b<>c/\a
       c/\/        / /
       \\/        \\/
-}
test_diamond_left_join :: GenEdit d => d -> Gen Property
test_diamond_left_join d = do
    (a, b, d', c) <- genPatchesForDiamondJoin d
    failPrint (a, b, c) $ equalEdit (fst $ merge d' c (fst $ merge d b a)) (fst $ merge d (b <> c) a) (patch c d')

test_inverse :: GenEdit d => d -> Gen Property
test_inverse d = do
    a <- genEdit d
    failPrint (a, inverse d a) $ equalEdit (a <> inverse d a) mempty d

test_inverse_inverse :: GenEdit d => d -> Gen Property
test_inverse_inverse d = do
    a <- genEdit d
    failPrint a $ equalEdit (inverse (patch a d) (inverse d a)) a d

-- | this law is derivable
test_inverse_diamond :: GenEdit d => d -> Gen Property
test_inverse_diamond d = do
    a <- genEdit d
    b <- genEdit d
    let (a2, b2) = merge d a b
        d' = patch (a <> a2) d
    failPrint (a, b) $ equalEdit (inverse d (a <> a2)) (inverse d (b <> b2)) d'

test_diff :: GenEdit d => d -> Gen Property
test_diff d = do
    p <- genEdit d
    let p' = diff d (patch p d)
    failPrint (p, patch p d, p') $ either (const True) (\pp -> equalEdit p pp d) p'

test_diff2 :: GenEdit d => d -> Gen Property
test_diff2 d = do
    d' <- arbitrary
    let p' = diff d d'
    failPrint (d', p') $ either (const True) (\pp -> patch pp d == d') p'

---------------------------------------- () instance

instance GenEdit () where
    genEdit _ = pure []

---------------------------------------- NonEditable instance

instance (Eq a, Show a, Arbitrary a) => GenEdit (NonEditable a) where
    genEdit _ = pure []

---------------------------------------- (,) instance

sizedEdit :: Gen [a] -> Gen [a]
sizedEdit m = sized $ \case
    0 -> pure []
    _ -> oneof [pure [], scale (`div` 2) m]

instance (GenEdit a, GenEdit b) => GenEdit (a, b) where
    genEdit (a, b) = sizedEdit $ oneof
        [ editFirst  <$> genEdit a
        , editSecond <$> genEdit b
        ]

---------------------------------------- Either instance

instance (GenEdit a, GenEdit b) => GenEdit (Either a b) where
    genEdit = \case
        Left a -> sizedEdit $ oneof
            [ editLeft <$> genEdit a
            , pure . SetEither <$> arbitrary
            ]
        Right b -> sizedEdit $ oneof
            [ editRight <$> genEdit b
            , pure . SetEither <$> arbitrary
            ]

---------------------------------------- (Bounded, Enum) instance

instance (Eq a, Show a, Arbitrary a) => GenEdit (Atom a) where
    genEdit _ = fmap EAtom <$> listOf (scale (`div` 2) arbitrary)

---------------------------------------- Char instance (via Atom Char)

instance GenEdit Char where
    genEdit = fmap (map EChar) . genEdit . Atom

---------------------------------------- List instance

instance (GenEdit a) => GenEdit [a] where
    genEdit d = sizedEdit $ do
            c <- genEdit d
            let d' = patch c d
                n = length d'
            oneof $
                    [ do
                        ch <- arbitrary
                        pure $ c <> [InsertItem i ch]
                    | i <- [0..n]]
                 <> [ do
                        l <- choose (1, n-i)
                        pure $ c `appendListEdit` deleteRange i l
                    | i <- [0..n-1]]
                 <> [ do
                        cx <- genEdit x
                        pure $ c <> editItem i cx
                    | (i, x) <- zip [0..] d']

---------------------------------------- non-empty list instance

instance GenEdit a => GenEdit (NonEmpty a) where
    genEdit (NEL.toList -> s) = do
        e <- genEdit s `suchThat` maintainsNonEmpty s
        pure $ coerce e

---------------------------------------- Seq instance

instance (GenEdit a) => GenEdit (Seq a) where
    genEdit d = sizedEdit $ do
            c <- genEdit d
            let d' = patch c d
                n = length d'
            oneof $
                    [do
                        ch <- arbitrary
                        pure $ c <> [InsertSItem i ch] | i <- [0..n]]
                 <> [pure $ c <> [DeleteSItem i] | i <- [0..n-1]]
                 <> [ do
                        cx <- genEdit x
                        pure $ c <> editSItem i cx
                    | (i, x) <- zip [0..] $ foldr (:) [] d']

---------------------------------------- Strict text instance

instance GenEdit ST where
    genEdit = fmap (map EText) . genEdit . ST.unpack


---------------------------------------- non-empty Strict text instance

instance GenEdit NonEmptyST where
    genEdit (NonEmptyST s) = do
        e <- genEdit s `suchThat` maintainsInvariant (not . ST.null) s
        pure $ coerce e

---------------------------------------- Set instance

instance (GenEdit a, Ord a, HasEnoughInhabitants a, Eq (EEdit a)) => GenEdit (Set a) where
    genEdit d = sizedEdit $ do
            c <- genEdit d
            let d' = patch c d
            oneof $
                    [ do
                        x <- arbitrary `suchThat` (`Set.notMember` d')
                        pure $ c <> [InsertElem x] | hasSpace d']
                 <> [ pure $ c <> [DeleteElem x] | x <- Set.elems d']
                 <> [ do
                        cx <- genEdit x `suchThat` \cx -> patch cx x `Set.notMember` d'
                        pure $ c <> editElem x cx
                    | hasSpace d', x <- Set.elems d']
      where
        hasSpace s = hasMoreInhabitantsThan (Proxy :: Proxy a) (Set.size s)

-- | Auxiliary class to ensure that a type have enough inhabitants
--   used for generating random elements
class HasEnoughInhabitants a where
    -- | Number of inhabitants; Nothing means a lot (more than 1000)
    numOfInhabitants :: Proxy a -> Maybe Int
    default numOfInhabitants :: (Enum a, Bounded a) => Proxy a -> Maybe Int
    numOfInhabitants _ = Just $ fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1

hasMoreInhabitantsThan :: (HasEnoughInhabitants a) => Proxy a -> Int -> Bool
hasMoreInhabitantsThan p n = maybe True (n <) $ numOfInhabitants p

instance HasEnoughInhabitants [a] where numOfInhabitants _ = Nothing
instance HasEnoughInhabitants a => HasEnoughInhabitants (Set a) where
    numOfInhabitants _ = do
        n <- numOfInhabitants (Proxy :: Proxy a)
        guard (n < 10)  -- to prevent overflow
        pure $ 2^n

---------------------------------------- Segments instance

instance (GenEdit a, GenEdit b, Splitable b) => GenEdit (Segments a b) where
    genEdit d = sizedEdit $ do
            c <- genEdit d
            let (Segments d') = patch c d
                n = length d'
            oneof $
                    [ do
                        ch <- arbitrary
                        pure $ c <> [SegmentListEdit $ InsertItem i ch]
                    | i <- [0..n]]
                 <> [ do
                        l <- choose (1, n-i)
                        pure $ c <> (SegmentListEdit <$> deleteRange i l)
                    | i <- [0..n-1]]
                 <> [ do
                        cx <- genEdit x
                        pure $ c <> (SegmentListEdit <$> editItem i cx)
                    | (i, x) <- zip [0..] d']
                 <> [ pure $ c <> (SegmentListEdit <$> editItem i (editFirst di)) <> [JoinItems i]
                    | (i, (x, _), (y, _)) <- zip3 [0..] d' (drop 1 d'), di <- either (const []) (:[]) $ diff x y]
                 <> [do
                        j <- choose (0, jmax)
                        pure $ c <> [SplitItem i j]
                    | (i, (_, x)) <- zip [0..] d', let jmax = maxSplitIndex x, jmax >= 0]

---------------------- test Splitable type class laws

testSplitable :: forall d. (Typeable d, Splitable d, Eq d, Arbitrary d, Show d) => Proxy d -> Spec
testSplitable p
    = describe ("Splitable instance for " <> show (typeRep p)) $ do
        it "splitLength >= 0"    $ property test_splitLength
        it "maxSplitIndex >= -1" $ property test_maxSplitIndex
        it "join . split == id"  $ property join_split
        it "split . join == id"  $ property split_join
  where
    test_splitLength :: d -> Bool
    test_splitLength d = splitLength d >= 0

    test_maxSplitIndex :: d -> Bool
    test_maxSplitIndex d = maxSplitIndex d >= (-1)

    join_split :: d -> Gen Bool
    join_split a = case maxSplitIndex a of
        -1 -> pure True
        ms -> do
            i <- choose (0, ms)
            pure $ uncurry joinItems (splitItem i a) == a

    split_join :: d -> d -> Bool
    split_join a b = splitItem (splitLength a) (joinItems a b) == (a, b)

---------------------- data type used for testing

data Digit = D1 | D2 | D3 | D4 | D5
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Digit where
    arbitrary = elements [minBound..]

type ADigit = Atom Digit

instance HasEnoughInhabitants ADigit

spec :: Spec
spec = parallel $ do
    runTest $ allTests @()
    runTest $ allTests @(NonEditable Char)
    runTest $ allTests @ADigit
    runTest $ allTests @(ADigit, ADigit)
    runTest $ allTests @(ADigit, (ADigit, ADigit))
    runTest $ allTests @(Either ADigit ADigit)
    runTest $ allTests @(Either ADigit (Either ADigit ADigit))
    runTest $ allTests @(Either ADigit ADigit, ADigit)
    runTest $ allTests @(Either ADigit (ADigit, ADigit))
    runTest $ allTests @[ADigit]
    runTest $ allTests @[(ADigit, ADigit)]
    runTest $ allTests @([ADigit], [ADigit])
    runTest $ allTests @(NonEmpty ADigit)
    runTest $ allTests @(NonEmpty (ADigit, ADigit))
    runTest $ allTests @(NonEmpty ADigit, NonEmpty ADigit)
    runTest $ allTests @(Seq ADigit)
    runTest $ allTests @(Seq (ADigit, ADigit))
    runTest $ allTests @(Seq ADigit, Seq ADigit)
    runTest $ allTests @(Set ADigit)
    runTest $ allTests @(Set [ADigit])
    runTest $ allTests @[Set ADigit]
    runTest $ allTests @(Set (Set ADigit))
    runTest $ allTests @ST
    runTest $ allTests @NonEmptyST
    runTest $ allTests @Char
    runTest $ allTests @(Segments ADigit ST)
    runTest $ allTests @(Segments ADigit NonEmptyST)

    runTest $ fastTests @[[ADigit]]

    -- these take too long to run on a regular basis, just activate for debugging or deep-tests:
    -- runTest' 500 $ allTests @[[ADigit]]

    testSplitable $ Proxy @[ADigit]
    testSplitable $ Proxy @(NEL.NonEmpty ADigit)
    testSplitable $ Proxy @(Seq ADigit)
    testSplitable $ Proxy @ST
    testSplitable $ Proxy @NonEmptyST
    testSplitable $ Proxy @(Segments ADigit ST)
    testSplitable $ Proxy @(Segments ADigit NonEmptyST)

-- | running in ghci8 on a lenovo t420s with no attempt at optimizing, @n = 1000@: @(2.88 secs,
-- 2,382,007,256 bytes)@.  this should be our baseline from which to improve.
--
-- more data points (same setup):
-- - Mon Jun  5 12:08:03 CEST 2017, commit 2968d504760e: @(10.90 secs, 3,919,324,968 bytes)@
-- This is a regression, most probably caused by changing the type signature of 'diff'.
-- FUTUREWORK:
--    re-write diff from scratch, adding the ability to detect line swaps + make it faster (divip has some ideas)
simplePerformanceBenchmark :: IO ()
simplePerformanceBenchmark = let n = 1000 in print (diff (take n ['a'..]) (take n ['A'..]) :: Either String (Edit String))
