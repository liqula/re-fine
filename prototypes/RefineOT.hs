{-
TODO
-   move code to refine
-   smarter Editable instance for Doc
-   separate the common parts into a library

FUTUREWORK
-   measure information lost during merge
-}

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
module RefineOT where

import Data.Monoid
import Data.Function
import Data.List
import qualified Data.Vector as Vec
import Control.Arrow
import Control.Monad
import Test.QuickCheck
import Data.Typeable

----------------------------------------------------------------------------------------------

-- [a, b, c] means first a, then b and c
type Edit d = [EEdit d]

class Editable d where

    docCost :: d -> Int

    data EEdit d            -- elementary edit

    eCost :: EEdit d -> Int

    diff   :: d -> d -> Edit d

    ePatch :: EEdit d -> d -> d

    -- assume second happend later in case of conflicts
    eMerge :: d -> EEdit d -> EEdit d -> (Edit d, Edit d)
    eMerge = secondWins

    eInverse :: d -> EEdit d -> Edit d        -- needed for supporting undo/redo

cost :: Editable d => Edit d -> Int
cost = sum . map eCost

patch :: Editable d => Edit d -> d -> d
patch = foldr (flip (.)) id . map ePatch

-- warning: this is at least quadratic, use with care
-- a >< b = (a x b, b x a) with 'a' having precedence
merge :: Editable d => d -> Edit d -> Edit d -> (Edit d, Edit d)
merge _ [] b = (b, [])
merge _ b [] = ([], b)
merge d (a0: a1) (b0: b1) = (b0a0a1 <> b1a0b0a1b0a0, a0b0b1 <> a1b0a0b1a0b0)
  where
    (b0a0, a0b0) = eMerge d a0 b0
    (b0a0a1, a1b0a0) = merge (ePatch a0 d) a1 b0a0
    (b1a0b0, a0b0b1) = merge (ePatch b0 d) a0b0 b1
    (b1a0b0a1b0a0, a1b0a0b1a0b0) = merge (patch (a0: b0a0) d) a1b0a0 b1a0b0

inverse :: Editable d => d -> Edit d -> Edit d
inverse = f []
  where
    f acc _ [] = acc
    f acc d (x: xs) = f (eInverse d x <> acc) (ePatch x d) xs

-- good for default eMerge
secondWins :: Editable d => d -> EEdit d -> EEdit d -> (Edit d, Edit d)
secondWins d a b = (eInverse d a <> [b], []) -- information lost!

---------------------------------------- quickcheck laws

class (Editable d, Arbitrary d, Eq d, Show d, Show (EEdit d)) => GenEdit d where
    genEdit :: d -> Gen (Edit d)

test_all :: forall d. GenEdit d => Int -> Proxy d -> IO ()
test_all num _ = do
  let args = stdArgs { maxSuccess = num }
      tests =
        [ test_edit_composition
        , test_diamond
        , test_diamond_right_join
        , test_diamond_left_join
        , test_inverse
        , test_inverse_inverse
        , test_inverse_diamond
        , test_diff
        ]
        :: [d -> Gen Property]
  forM_ tests $ \f -> do
    quickCheckWith args f

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
  a <- genEdit d
  b <- genEdit d
  let d' = patch b d
  c <- genEdit d'
  failPrint (a, b, c) $ equalEdit (snd $ merge d' (snd $ merge d a b) c) (snd $ merge d a (b <> c)) (patch c d')

{- mirror of test_diamond_right_join
        b/\a  = b<>c/\a
       c/\/        / /
       \\/        \\/
-}
test_diamond_left_join :: GenEdit d => d -> Gen Property
test_diamond_left_join d = do
  a <- genEdit d
  b <- genEdit d
  let d' = patch b d
  c <- genEdit d'
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

---------------------------------------- List instance

editItem :: Int -> Edit a -> Edit [a]
editItem _ [] = []
editItem i p  = [EditItem i p]

instance Editable a => Editable [a] where

    docCost = (+1) . sum . map docCost

    data EEdit [a]
        = DeleteItem Int
        | InsertItem Int a
        | EditItem Int (Edit a)
        -- not used yet
        --  MoveItem Int Int     -- needed for expressing .....
        --  DuplicateItem Int

    eCost = \case
        InsertItem _ d -> 1 + docCost d
        DeleteItem _   -> 1
        EditItem _ p   -> 1 + cost p

    ePatch (DeleteItem i  ) xs = take i xs ++ drop (i+1) xs
    ePatch (InsertItem i x) xs = take i xs ++ x: drop i xs
    ePatch (EditItem   i x) xs = take i xs ++ patch x (xs !! i): drop (i+1) xs

    diff s1 s2 = snd $ f' $ length s1 * xx + nmax
      where
        table :: Vec.Vector (Int, Edit [a])
        table = Vec.fromList $ zipWith f_ [0..] $ (,) <$> reverse (tails s1) <*> reverse (tails $ zip [0..] s2)

        f' i = table Vec.! i
        nmax = length s2
        xx = nmax + 1

        f_ a (b, c) = f a b c

        f _ [] [] = mk []
        f j [] ((n,x): _) = mk [InsertItem n x] `add` f' (j-1)
        f i (_: _) [] = mk [DeleteItem nmax] `add` f' (i-xx)
        f ij (x: _) ((n, y): _)
            | null dxy = f' (ij-xx-1)
            | otherwise = minimumBy (compare `on` fst)
                [ mk (editItem n dxy) `add` f' (ij-xx-1)
                , mk [InsertItem n y] `add` f' (ij-1)
                , mk [DeleteItem n]   `add` f' (ij-xx)
                ]
          where
            dxy = diff x y

        mk x = (cost x, x)
        (ca, a) `add` (cb, b) = (ca + cb, a <> b)

    eMerge d (EditItem i x) (EditItem i' y) | i == i' = ([EditItem i x2], [EditItem i y2])
      where
        (x2, y2) = merge (d !! i) x y
    eMerge _ (EditItem i _) (DeleteItem i') | i == i' = ([DeleteItem i], [])      -- FUTUREWORK: information lost!
    eMerge d (DeleteItem i) (EditItem i' x) | i == i' = ([InsertItem i (d !! i), EditItem i x], [])
    eMerge _ (DeleteItem i) (DeleteItem i') | i == i' = ([], [])
    eMerge _ a b = (modify 0 a b, modify 1 b a)
      where
        modify l e = \case
            InsertItem i x -> [InsertItem (di l i) x]
            DeleteItem i   -> [DeleteItem (di 1 i)  ]
            EditItem   i x -> [EditItem   (di 1 i) x]
          where
            di k i = case e of
                InsertItem j _ -> if j==i then i+k else if j<i then i+1 else i
                DeleteItem j   -> if j==i then i else if j<i then i-1 else i
                EditItem{}     -> i
{-
edi 0 p `merge` del 0   = (del 0  , []     )               --      x... --> ...
edi 0 p `merge` del 0   = ([]     , ins 0 x <> edi 0 p)    --      x... --> p...

ins 0 a `merge` ins 0 b = (ins 0 b, ins 1 a)    --      ba...
ins 0 a `merge` ins 0 b = (ins 1 b, ins 0 a)    --      ab...

ins 0 a `merge` del 0   = (del 1  , ins 0 a)

del 0   `merge` ins 0 b = (ins 0 b, del 1  )

ins 0 a `merge` edi 0 p = (edi 1 p, ins 0 a)                        --      ap...
ins 0 a `merge` edi 0 p = (del 0 <> ins 1 a <> edi 0 p, ins 1 a)    --      x... -> ax... -> Xa...
ins 0 a `merge` edi 0 p = (swap 0 <> edi 0 p, ins 1 a)              --      pa...
-}

    eInverse d = \case
        EditItem i x   -> [EditItem i (inverse (d !! i) x)]
        DeleteItem i   -> [InsertItem i (d !! i)]
        InsertItem i _ -> [DeleteItem i]

instance (GenEdit a) => GenEdit [a] where
    genEdit d = oneof
        [ pure []
        , do
            c <- genEdit d
            ch <- arbitrary
            let d' = patch c d
                n = length d'
            oneof $
                    [pure $ c ++ [InsertItem i ch] | i <- [0..n]]
                 ++ [pure $ c ++ [DeleteItem i] | i <- [0..n-1]]
                 ++ [ do
                        cx <- genEdit x
                        pure $ c ++ [EditItem i cx]
                    | (i, x) <- zip [0..] d']
        ]

deriving instance (Show a, Show (EEdit a)) => Show (EEdit [a])

---------------------------------------- (Bounded, Enum) instance

newtype Atom a = Atom { unAtom :: a }
  deriving (Eq, Ord, Bounded, Enum)

instance Show a => Show (Atom a) where show = show . unAtom

instance (Eq a, Bounded a, Enum a) => Editable (Atom a) where
    newtype EEdit (Atom a) = ReplaceEnum (Atom a)
      deriving (Show)

    eCost _ = 1
    docCost _ = 1

    diff a b = [ReplaceEnum b | a /= b]
    ePatch (ReplaceEnum a) _ = a
    eMerge _ ReplaceEnum{} b@ReplaceEnum{} = ([b], mempty)
    eInverse d ReplaceEnum{} = [ReplaceEnum d]

instance (Bounded a, Enum a) => Arbitrary (Atom a) where
  arbitrary = Atom <$> elements [minBound..]

instance (Eq a, Show a, Arbitrary (Atom a), Bounded a, Enum a) => GenEdit (Atom a) where
    genEdit _ = fmap ReplaceEnum <$> listOf arbitrary

---------------------------------------- Char instance (via Atom Char)

instance Editable Char where
    newtype EEdit Char = EChar {unEChar :: EEdit (Atom Char)}
        deriving (Show)
    docCost = docCost . Atom
    eCost = eCost . unEChar
    diff a b = map EChar $ diff (Atom a) (Atom b)
    ePatch e = unAtom . ePatch (unEChar e) . Atom
    eMerge d a b = map EChar *** map EChar $ eMerge (Atom d) (unEChar a) (unEChar b)
    eInverse d = map EChar . eInverse (Atom d) . unEChar

instance GenEdit Char where
    genEdit = fmap (map EChar) . genEdit . Atom

---------------------------------------- (,) instance

editFirst :: Edit a -> Edit (a, b)
editFirst [] = []
editFirst e  = [EditFirst e]

editSecond :: Edit b -> Edit (a, b)
editSecond [] = []
editSecond e  = [EditSecond e]

instance (Editable a, Editable b) => Editable (a, b) where

    docCost (a, b) = 1 + docCost a + docCost b

    data EEdit (a, b)
        = EditFirst  (Edit a)
        | EditSecond (Edit b)

    eCost = \case
        EditFirst  e -> 1 + cost e
        EditSecond e -> 1 + cost e

    ePatch (EditFirst  e) (a, b) = (patch e a, b)
    ePatch (EditSecond e) (a, b) = (a, patch e b)

    diff (a, b) (c, d) = editFirst (diff a c) <> editSecond (diff b d)

    eMerge _ (EditFirst ea) (EditSecond eb) = (editSecond eb, editFirst ea)
    eMerge _ (EditSecond eb) (EditFirst ea) = (editFirst ea, editSecond eb)
    eMerge (a, _) (EditFirst e) (EditFirst f) = (editFirst e2, editFirst f2)
      where (e2, f2) = merge a e f
    eMerge (_, b) (EditSecond e) (EditSecond f) = (editSecond e2, editSecond f2)
      where (e2, f2) = merge b e f

    eInverse (a, b) = \case
        EditFirst  e -> editFirst  $ inverse a e
        EditSecond e -> editSecond $ inverse b e

instance (GenEdit a, GenEdit b) => GenEdit (a, b) where
    genEdit (a, b) = oneof
        [ pure []
        , pure . EditFirst  <$> genEdit a
        , pure . EditSecond <$> genEdit b
        ]

deriving instance (Show a, Show (EEdit a), Show b, Show (EEdit b)) => Show (EEdit (a, b))

---------------------------------------- Either instance

editLeft :: Edit a -> Edit (Either a b)
editLeft [] = []
editLeft e  = [EditLeft e]

editRight :: Edit b -> Edit (Either a b)
editRight [] = []
editRight e  = [EditRight e]

instance (Editable a, Editable b) => Editable (Either a b) where

    docCost (Left a)  = 1 + docCost a
    docCost (Right b) = 1 + docCost b

    data EEdit (Either a b)
        = EditLeft  (Edit a)
        | EditRight (Edit b)
        | SetEither (Either a b)

    eCost = \case
        EditLeft  e -> 1 + cost e
        EditRight e -> 1 + cost e
        SetEither x -> 1 + either docCost docCost x

    ePatch (EditLeft  e) (Left  a) = Left  (patch e a)
    ePatch (EditRight e) (Right b) = Right (patch e b)
    ePatch (SetEither x) _         = x

    diff (Left  a) (Left  a') = editLeft  $ diff a a'
    diff (Right b) (Right b') = editRight $ diff b b'
    diff _ x = [SetEither x]

    eMerge (Left  a) (EditLeft  ea) (EditLeft  ea') = editLeft  *** editLeft  $ merge a ea ea'
    eMerge (Right b) (EditRight eb) (EditRight eb') = editRight *** editRight $ merge b eb eb'
    eMerge d a b = secondWins d a b

    eInverse (Left  a) (EditLeft  e) = editLeft  $ inverse a e
    eInverse (Right b) (EditRight e) = editRight $ inverse b e
    eInverse x         (SetEither _) = [SetEither x]

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

deriving instance (Show a, Show (EEdit a), Show b, Show (EEdit b)) => Show (EEdit (Either a b))

---------------------------------------- () instance

instance Editable () where

    docCost _  = 1

    data EEdit ()

    eCost _ = error "impossible"
    ePatch _ _ = error "impossible"
    diff _ _ = []
    eMerge _ _ _ = error "impossible"
    eInverse _ _ = error "impossible"

instance GenEdit () where
    genEdit _ = pure []

instance Show (EEdit ()) where show = error "impossible"

---------------------------------------- Set instance

newtype Set a = Set {unSet :: [a]}
   deriving (Show, Eq, Ord)

instance Ord a => Monoid (Set a) where
    mempty = Set []
    mappend (Set a) (Set b) = Set $ mergeList a b

mergeList :: Ord a => [a] -> [a] -> [a]
mergeList [] xs = xs
mergeList xs [] = xs
mergeList (x: xs) (y: ys) = case compare x y of
    LT -> x: mergeList xs (y: ys)
    GT -> y: mergeList (x: xs) ys
    _  -> x: mergeList xs ys

editElem :: a -> Edit a -> Edit (Set a)
editElem _ [] = []
editElem i p  = [EditElem i p]

instance (Editable a, Ord a) => Editable (Set a) where

    docCost (Set x) = (+1) . sum $ map docCost x

    data EEdit (Set a)
        = DeleteElem a
        | InsertElem a          -- it is not valid to insert an elem which is already in the set
        | EditElem a (Edit a)   -- it is not valid to edit such that the result is already in the set

    eCost = \case
        InsertElem d -> 1 + docCost d
        DeleteElem _ -> 1
        EditElem _ e -> 1 + cost e

    ePatch (DeleteElem x) (Set xs) = Set $ filter (/=x) xs
    ePatch (InsertElem x) (Set xs) = Set $ insert x xs
    ePatch (EditElem x e) (Set xs) = Set $ insert (patch e x) $ filter (/=x) xs

    diff (Set a) (Set b) = f a b
      where
        f [] [] = []
        f [] (x:xs) = InsertElem x: f [] xs
        f (x:xs) [] = DeleteElem x: f xs []
        f (x:xs) (y:ys) = case compare x y of
            EQ -> f xs ys
            LT -> DeleteElem x: f xs (y: ys)
            GT -> InsertElem y: f (x: xs) ys

    eMerge _ a b | a == b = ([], [])
    eMerge d a@(EditElem i x) b@(EditElem i' y) | i == i'
        = if patch (x <> x2) i `notElem` unSet d
            then (editElem (patch x i) x2, editElem (patch y i) y2)
            else secondWins d a b
      where
        (x2, y2) = merge i x y
    eMerge d a@(EditElem i x) b@(EditElem i' y) | i /= i' && patch x i == patch y i' = secondWins d a b
    eMerge _ (EditElem i x) (DeleteElem i') | i == i' = ([DeleteElem (patch x i)], [])      -- FUTUREWORK: information lost!
    eMerge _ (DeleteElem i) (EditElem i' x) | i == i' = ([InsertElem (patch x i)], [])
    eMerge _ (InsertElem x) (EditElem y e) | x == patch e y = if x == y then ([], []) else ([DeleteElem y], [])
    eMerge d a@(EditElem y e) b@(InsertElem x) | x == patch e y = if x == y then ([], []) else secondWins d a b
    eMerge _ a b = ([b], [a])

    eInverse _ = \case
        EditElem x e -> [EditElem (patch e x) (inverse x e)]
        DeleteElem x -> [InsertElem x]
        InsertElem x -> [DeleteElem x]

deriving instance (Show a, Show (EEdit a)) => Show (EEdit (Set a))

instance (Eq a, Editable a) => Eq (EEdit (Set a)) where
    InsertElem a == InsertElem b = a == b
    DeleteElem a == DeleteElem b = a == b
    EditElem a ea == EditElem b eb = a == b && patch ea a == patch eb b
    _ == _ = False

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
                        pure $ c ++ [InsertElem x] | hasSpace d']
                 ++ [ pure $ c ++ [DeleteElem x] | x <- unSet d']
                 ++ [ do
                        cx <- genEdit x `suchThat` \cx -> patch cx x `notElem` unSet d'
                        pure $ c ++ [EditElem x cx]
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

------------------------------------------------------- auxiliary definitions

class Representable a where
  type Rep a
  to   :: Rep a -> a
  from :: a -> Rep a

---------------------- data type used for testing

data Digit = D1 | D2 | D3 | D4 | D5
    deriving (Eq, Ord, Show, Enum, Bounded)

type ADigit = Atom Digit

allTests :: IO ()
allTests = do
    test_all 5000 (Proxy :: Proxy [ADigit])
    test_all 50   (Proxy :: Proxy [[ADigit]])

    test_all 5000 (Proxy :: Proxy ADigit)

    test_all 5000 (Proxy :: Proxy (ADigit, ADigit))
    test_all 5000 (Proxy :: Proxy (ADigit, (ADigit, ADigit)))
    test_all 500  (Proxy :: Proxy ([ADigit], [ADigit]))
    test_all 500  (Proxy :: Proxy [(ADigit, ADigit)])

    test_all 5000 (Proxy :: Proxy ())
    test_all 5000 (Proxy :: Proxy (Either () ()))
    test_all 5000 (Proxy :: Proxy (Either ADigit ADigit))

    test_all 500  (Proxy :: Proxy (Set ADigit))
    -- test_all 50 (Proxy :: Proxy (Set (Set ADigit)))

main :: IO ()
main = do
    --allTests
    -- | performance benchmark
    let n = 1000 in print $ diff (take n ['a'..]) (take n ['A'..])
