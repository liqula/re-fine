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

import           Data.Monoid
import           Data.Typeable
import qualified Data.Set as Set
import           Control.Monad
import           Test.QuickCheck
import           Test.Hspec
import qualified Data.Text as Text

import Refine.Common.OT
import Refine.Common.Test.Arbitrary ()

---------------------------------------- quickcheck laws

-- | Auxiliary class needed for testing only
class (Editable d, Arbitrary d, Eq d, Show d, Show (EEdit d)) => GenEdit d where
    genEdit :: d -> Gen (Edit d)

runTest :: forall d. (Typeable d, GenEdit d) => [(String, d -> Gen Property)] -> Spec
runTest = runTest' 1

runTest' :: forall d. (Typeable d, GenEdit d) => Int -> [(String, d -> Gen Property)] -> Spec
runTest' scaleFactor tests
    = describe ("Editable instance for " <> show (typeRep (Proxy :: Proxy d)))
    . forM_ tests $ \(name, test) -> it name
    $ property (scale (`div` scaleFactor) <$> test) -- quickCheckWith stdArgs { maxSuccess = num }

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
    failPrint a $ equalEdit (a <> inverse d a) mempty d

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
    failPrint (p, patch p d, p', patch p' d) $ equalEdit p p' d

---------------------------------------- () instance

instance GenEdit () where
    genEdit _ = pure []

---------------------------------------- (,) instance

instance (GenEdit a, GenEdit b) => GenEdit (a, b) where
    genEdit (a, b) = oneof
        [ pure []
        , pure . EditFirst  <$> genEdit a
        , pure . EditSecond <$> genEdit b
        ]

---------------------------------------- Either instance

instance (GenEdit a, GenEdit b) => GenEdit (Either a b) where
    genEdit = \case
        Left a -> oneof
            [ pure []
            , pure . EditLeft  <$> genEdit a
            , pure . SetEither <$> arbitrary
            ]
        Right b -> oneof
            [ pure []
            , pure . EditRight <$> genEdit b
            , pure . SetEither <$> arbitrary
            ]

---------------------------------------- (Bounded, Enum) instance

instance (Eq a, Show a, Arbitrary a) => GenEdit (Atom a) where
    genEdit _ = fmap EAtom <$> listOf arbitrary

deriving instance Arbitrary a => Arbitrary (Atom a)

---------------------------------------- Char instance (via Atom Char)

instance GenEdit Char where
    genEdit = fmap (map EChar) . genEdit . Atom

---------------------------------------- List instance

instance (GenEdit a) => GenEdit [a] where
    genEdit d = oneof
        [ pure []
        , do
            c <- genEdit d
            ch <- arbitrary
            let d' = patch c d
                n = length d'
            oneof $
                    [pure $ c <> [InsertItem i ch] | i <- [0..n]]
                 <> [pure $ c <> [DeleteItem i] | i <- [0..n-1]]
                 <> [ do
                        cx <- genEdit x
                        pure $ c <> [EditItem i cx]
                    | (i, x) <- zip [0..] d']
        ]

---------------------------------------- Strict text instance

instance GenEdit Text.Text where
    genEdit = fmap (map EText) . genEdit . Text.unpack


---------------------------------------- Set instance

instance (GenEdit a, Ord a, HasEnoughInhabitants a) => GenEdit (Set.Set a) where
    genEdit d = oneof
        [ pure []
        , do
            c <- genEdit d
            let d' = patch c d
            oneof $
                    [ do
                        x <- arbitrary `suchThat` (`Set.notMember` d')
                        pure $ c <> [InsertElem x] | hasSpace d']
                 <> [ pure $ c <> [DeleteElem x] | x <- Set.elems d']
                 <> [ do
                        cx <- genEdit x `suchThat` \cx -> patch cx x `Set.notMember` d'
                        pure $ c <> [EditElem x cx]
                    | hasSpace d', x <- Set.elems d']
        ]
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

instance (Enum a, Bounded a) => HasEnoughInhabitants (Atom a)

instance HasEnoughInhabitants [a] where numOfInhabitants _ = Nothing
instance HasEnoughInhabitants a => HasEnoughInhabitants (Set.Set a) where
    numOfInhabitants _ = do
        n <- numOfInhabitants (Proxy :: Proxy a)
        guard (n < 10)  -- to prevent overflow
        pure $ 2^n

---------------------- data type used for testing

data Digit = D1 | D2 | D3 | D4 | D5
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Digit where
    arbitrary = elements [minBound..]

type ADigit = Atom Digit

spec :: Spec
spec = parallel $ do
    runTest $ allTests @()
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
    runTest $ allTests @(Set.Set ADigit)
    runTest $ allTests @(Set.Set [ADigit])
    runTest $ allTests @[Set.Set ADigit]
    runTest $ allTests @(Set.Set (Set.Set ADigit))
    runTest $ allTests @Text.Text
    runTest $ allTests @Char

    runTest $ fastTests @[[ADigit]]

    -- these take too long to run on a regular basis, just activate for debugging or deep-tests:
    -- runTest' 500 $ allTests @[[ADigit]]


-- | running in ghci8 on a lenovo t420s with no attempt at optimizing, @n = 1000@: @(2.88 secs,
-- 2,382,007,256 bytes)@.  this should be our baseline from which to improve.
simplePerformanceBenchmark :: IO ()
simplePerformanceBenchmark = let n = 1000 in print $ diff (take n ['a'..]) (take n ['A'..])
