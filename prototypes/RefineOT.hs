{-
TODO
-   Editable instace for Doc
-   tr1 :: Doc -> DDoc
    tr2 :: DDoc -> Doc
-   lots of tests & examples
-   separate the common parts into a library   -- Prezi could release this too?
-   move to code refine

FUTUREWORK
-   measure information lost
-   undo/redo support
-}

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE LambdaCase                 #-}
module RefineOT where

import Data.Monoid
import Data.Function
import Data.List
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.Typeable

---------------------------------------------------------------------------------------------- application independent part

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
inverse d e = f [] d e
  where
    f acc _ [] = acc
    f acc d (x: xs) = f (eInverse d x <> acc) (ePatch x d) xs

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
    failPrint (p, p') $ equalEdit p p' d

---------------------------------------- Char instance

instance Editable Char where

    docCost _ = 1

    data EEdit Char = ReplaceChar Char
            deriving (Show)

    eCost _ = 1

    diff a b | a == b    = mempty
             | otherwise = [ReplaceChar b]

    ePatch (ReplaceChar x) _ = x

    eMerge _ a@ReplaceChar{} b@ReplaceChar{} = ([b], mempty)

    eInverse c ReplaceChar{} = [ReplaceChar c]

instance GenEdit Char where
    genEdit d = oneof
        [ pure mempty
        , do
            c <- arbitrary
            (ReplaceChar c:) <$> genEdit d
        ]

test_Char = test_all 5000 (Proxy :: Proxy Char)

---------------------------------------- List instance

editItem i [] = []
editItem i p  = [EditItem i p]

instance Editable a => Editable [a] where

    docCost = sum . map docCost

    data EEdit [a]
        = DeleteItem Int
        | InsertItem Int a
        | EditItem Int (Edit a)
        | SwapItems Int     -- needed for expressing .....

    eCost = \case
        InsertItem _ d -> 1 + docCost d
        DeleteItem _   -> 1
        EditItem _ p   -> 1 + cost p

    ePatch (DeleteItem i  ) xs = take i xs ++ drop (i+1) xs
    ePatch (InsertItem i x) xs = take i xs ++ x: drop i xs
    ePatch (EditItem   i x) xs = take i xs ++ patch x (xs !! i): drop (i+1) xs

    diff a b = snd $ f 0 a b
      where
        f n [] [] = mk []
        f n [] (x:xs) = mk [InsertItem n x] `add` f (n+1) [] xs
        f n (x:xs) [] = mk [DeleteItem n] `add` f n xs []
        f n (x:xs) (y:ys)
            | null dxy = f (n+1) xs ys
            | otherwise = minimumBy (compare `on` fst)
                [ mk (editItem n dxy) `add` f (n+1) xs ys
                , mk [InsertItem n y] `add` f (n+1) (x:xs) ys
                , mk [DeleteItem n] `add` f n xs (y:ys)
                ]
          where
            dxy = diff x y

        mk x = (cost x, x)
        (ca, a) `add` (cb, b) = (ca + cb, a <> b)

    eMerge d (EditItem i x) (EditItem i' y) | i == i' = ([EditItem i x2], [EditItem i y2])
      where
        (x2, y2) = merge (d !! i) x y
    eMerge _ (EditItem i x) (DeleteItem i') | i == i' = ([DeleteItem i], [])      -- FUTUREWORK: information lost!
    eMerge d (DeleteItem i) (EditItem i' x) | i == i' = ([InsertItem i (d !! i), EditItem i x], [])
    eMerge _ (DeleteItem i) (DeleteItem i') | i == i' = ([], [])
    eMerge _ a b = (mod 0 a b, mod 1 b a)
      where
        mod k a b = case b of
            InsertItem i x -> [InsertItem (di k i) x]
            DeleteItem i   -> [DeleteItem (di 1 i)  ]
            EditItem   i x -> [EditItem   (di 1 i) x]
          where
            di k i = case a of
                InsertItem j _ -> if j==i then i+k else if j<i then i+1 else i
                DeleteItem j   -> if j==i then i else if j<i then i-1 else i
                EditItem j _   -> i
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

test_String = test_all 5000 (Proxy :: Proxy [Char])
test_StringList = test_all 500 (Proxy :: Proxy [[Char]])
test_StringListList = test_all 50 (Proxy :: Proxy [[[Char]]])

---------------------------------------- Pair instance

editFirst [] = []
editFirst e  = [EditFirst e]

editSecond [] = []
editSecond e  = [EditSecond e]

instance (Editable a, Editable b) => Editable (a, b) where

    docCost (a, b) = docCost a + docCost b

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

test_PairCharChar = test_all 5000 (Proxy :: Proxy (Char, Char))
test_PairCharPairCharChar = test_all 5000 (Proxy :: Proxy (Char, (Char, Char)))
test_PairStringString = test_all 500 (Proxy :: Proxy ([Char], [Char]))
test_PairCharCharList = test_all 500 (Proxy :: Proxy [(Char, Char)])

---------------------------------------------------------------------------------------------- application specific part

---------------------------------------- document data type

newtype Doc = Doc [Block]
   deriving (Show, Eq)

data Block = Block BlockType [LineElem]
   deriving (Show, Eq)

data BlockType =
     Header HeaderLevel
   | Item ItemType Int -- depth
   deriving (Show, Eq)

data HeaderLevel
    = HL1 | HL2 | HL3
   deriving (Show, Eq)

data ItemType = NormalText | BulletPoint | EnumPoint
   deriving (Show, Eq)

data LineElem = LineElem (Set Entity) String
   deriving (Show, Eq)

type Set a = [a]  -- TODO: this should be a newtype

-- | This is both Entity and Style in Draft
data Entity
    = EntityLink String
    | EntityBold
    | EntityItalic
   deriving (Show, Eq)

    -- this is something which is described with an EntityRange
{-
  ------            bold
     --------       italic
xxxxxxxxxxxxxxxxxxxxxx converted to line elements:
  ---               bold
     ---            bold + italic
        -----       italic

 xxx [xxXXXx](www.1) xxxxx
 xxx [xx](www.1)[XXX](www.1)[x](www.1) xxxxx
-}

---------------- tests

doc1, doc2 :: Doc
doc1 = Doc
    [ Block (Header HL1) [LineElem [] "Introduction"]
    , Block (Item NormalText 0) [LineElem [] "This is the introduction"]
    ]

doc2 = Doc
    [ Block (Header HL1) [LineElem [] "Introduction"]
    , Block (Item NormalText 0) [LineElem [EntityBold] "This", LineElem [] " is the introduction"]
    ]


