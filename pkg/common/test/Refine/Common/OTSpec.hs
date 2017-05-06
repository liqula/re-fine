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

import Data.Monoid
import Data.List
import Control.Monad
import Test.QuickCheck
import Data.Typeable

import Test.Hspec

import Refine.Common.OT

---------------------------------------- quickcheck laws

-- | Auxiliary class needed for testing only
class (Editable d, Arbitrary d, Eq d, Show d, Show (EEdit d)) => GenEdit d where
    genEdit :: d -> Gen (Edit d)

runTest :: forall d. (Typeable d, GenEdit d) => [(String, d -> Gen Property)] -> Spec
runTest tests
    = describe ("Editable instance for  " <> show (typeRep (Proxy :: Proxy d)))
    . forM_ tests $ \(name, test) -> it name $ property test --quickCheckWith stdArgs { maxSuccess = num }

---------------------

allTestsButDiff :: GenEdit d => [(String, d -> Gen Property)]
allTestsButDiff =
    [ (,) "edit composition"    test_edit_composition
    , (,) "diamond"             test_diamond
    , (,) "diamond right join"  test_diamond_right_join
    , (,) "dimaond left join"   test_diamond_left_join
    , (,) "inverse"             test_inverse
    , (,) "inverse of inverse"  test_inverse_inverse
    , (,) "inverted diamond"    test_inverse_diamond
    ]

allTests :: GenEdit d => [(String, d -> Gen Property)]
allTests = allTestsButDiff <>
    [ (,) "diff" test_diff
    ]

---------------------

-- compare two edits by effect
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

{-
  a//\b  =  a/\\b
   \\/       \//
-}
test_diamond :: GenEdit d => d -> Gen Property
test_diamond d = do
    a <- genEdit d
    b <- genEdit d
    failPrint (a, b) $ equalEdit (a <> fst (merge d a b)) (b <> snd (merge d a b)) d

{-
        a/\b  =  a/\b<>c
         \/\c     \ \
          \//      \//
-}
test_diamond_right_join :: GenEdit d => d -> Gen Property
test_diamond_right_join d = do
    (a, b, d', c) <- genPatchesForDiamondJoin d
    failPrint (a, b, c) $ equalEdit (snd $ merge d' (snd $ merge d a b) c) (snd $ merge d a (b <> c)) (patch c d')

-- FIXME: I had to add this helper function only because hlint complained about duplicate code
genPatchesForDiamondJoin :: GenEdit d => d -> Gen (Edit d, Edit d, d, Edit d)
genPatchesForDiamondJoin d = do
    a <- genEdit d
    b <- genEdit d
    let d' = patch b d
    c <- genEdit d'
    pure (a, b, d', c)

{- mirror of test_diamond_right_join
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

-- this law is derivable
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

instance (Eq a, Show a, Arbitrary (Atom a), Bounded a, Enum a) => GenEdit (Atom a) where
    genEdit _ = fmap ReplaceEnum <$> listOf arbitrary

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

---------------------------------------- Set instance

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = Set . nub . getOrdered <$> arbitrary

instance (GenEdit a, Ord a, HasEnoughElems a) => GenEdit (Set a) where
    genEdit d = oneof
        [ pure []
        , do
            c <- genEdit d
            let d' = patch c d
            oneof $
                    [ do
                        x <- arbitrary `suchThat` (`notElem` unSet d')
                        pure $ c <> [InsertElem x] | hasSpace d']
                 <> [ pure $ c <> [DeleteElem x] | x <- unSet d']
                 <> [ do
                        cx <- genEdit x `suchThat` \cx -> patch cx x `notElem` unSet d'
                        pure $ c <> [EditElem x cx]
                    | hasSpace d', x <- unSet d']
        ]
      where
        hasSpace (Set x) = hasMoreElemsThan (Proxy :: Proxy a) (length x)

-- | Auxiliary class to ensure that a type have enough inhabitants
--   used for generating random elements
class HasEnoughElems a where
    hasMoreElemsThan :: Proxy a -> Int -> Bool
    default hasMoreElemsThan :: (Enum a, Bounded a) => Proxy a -> Int -> Bool
    hasMoreElemsThan _ n = n <= fromEnum (maxBound :: a) - fromEnum (minBound :: a)

instance (Enum a, Bounded a) => HasEnoughElems (Atom a)

instance HasEnoughElems [a] where hasMoreElemsThan _ _ = True
instance HasEnoughElems a => HasEnoughElems (Set a) where hasMoreElemsThan _ _ = True  -- FIXME

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
    runTest $ allTestsButDiff @[[ADigit]]
    runTest $ allTests @(Set ADigit)
    runTest $ allTests @(Set [ADigit])
    runTest $ allTests @[Set ADigit]
    runTest $ allTests @(Set (Set ADigit))

main :: IO ()
main = do
    --allTests
    -- | performance benchmark
    let n = 1000 in print $ diff (take n ['a'..]) (take n ['A'..])
