{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE BangPatterns               #-}

-- | FUTUREWORK: release this file as a library
module Refine.Common.OT where

import           Data.Monoid
import qualified Data.Set as Set
import           Data.Function
import           Data.List
import           Control.Arrow
import qualified Data.Algorithm.Patience as Diff
import qualified Data.Text as Text

----------------------------------------------------------------------------------------------

-- | @[a, b, c]@ means first a, then b, then c
type Edit d = [EEdit d]

class Editable d where

    -- | Cost of a document
    docCost :: d -> Int

    -- | Elementary edit of `d`
    data EEdit d

    -- | Cost of an elementary edit
    eCost :: EEdit d -> Int

    diff   :: d -> d -> Edit d

    ePatch :: EEdit d -> d -> d

    -- | assume second happend later in case of conflicts
    -- FUTUREWORK: measure information lost during merge
    eMerge :: d -> EEdit d -> EEdit d -> (Edit d, Edit d)
    eMerge = secondWins

    -- | Inverse of an elementary edit (needed for supporting undo/redo)
    eInverse :: d -> EEdit d -> Edit d

    -- | FIXME: implement smarter compressEdit instances
    compressEdit :: d -> Edit d -> Edit d
    compressEdit _ = id

-- | Cost of an edit
cost :: Editable d => Edit d -> Int
cost = sum . map eCost

patch :: Editable d => Edit d -> d -> d
patch = foldr (flip (.) . ePatch) id

-- | warning: this is at least quadratic, use with care
--
-- >>> a >< b = (a x b, b x a)
--
-- with 'a' having precedence
merge :: Editable d => d -> Edit d -> Edit d -> (Edit d, Edit d)
merge _ [] b = (b, [])
merge _ b [] = ([], b)
merge d (a0: a1) (b0: b1) = (b0a0a1 <> b1a0b0a1b0a0, a0b0b1 <> a1b0a0b1a0b0)
  where
    (b0a0, a0b0) = eMerge d a0 b0
    (b0a0a1, a1b0a0) = merge (ePatch a0 d) a1 b0a0
    (b1a0b0, a0b0b1) = merge (ePatch b0 d) a0b0 b1
    (b1a0b0a1b0a0, a1b0a0b1a0b0) = merge (patch (a0: b0a0) d) a1b0a0 b1a0b0

-- | Inverse of an edit
inverse :: Editable d => d -> Edit d -> Edit d
inverse = f []
  where
    f acc _ [] = acc
    f acc d (x: xs) = f (eInverse d x <> acc) (ePatch x d) xs

-- | An 'eMerge' implementation that works for any instance.
-- Forget the first edit at merge.  This fulfills the laws but is a bit primitive.
secondWins :: Editable d => d -> EEdit d -> EEdit d -> (Edit d, Edit d)
secondWins d a b = (compressEdit d $ eInverse d a <> [b], []) -- information lost!

---------------------------------------- () instance

instance Editable () where
    data EEdit ()
    docCost _    = 1
    eCost _      = error "impossible"
    ePatch _ _   = error "impossible"
    diff _ _     = []
    eMerge _ _ _ = error "impossible"
    eInverse _ _ = error "impossible"

instance Show (EEdit ()) where show = error "impossible"

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
    eMerge (a, _) (EditFirst e)  (EditFirst f)  = editFirst  *** editFirst  $ merge a e f
    eMerge (_, b) (EditSecond e) (EditSecond f) = editSecond *** editSecond $ merge b e f

    eInverse (a, b) = \case
        EditFirst  e -> editFirst  $ inverse a e
        EditSecond e -> editSecond $ inverse b e

deriving instance (Show a, Show (EEdit a), Show b, Show (EEdit b)) => Show (EEdit (a, b))

---------------------------------------- Either instance

editLeft :: Edit a -> Edit (Either a b)
editLeft [] = []
editLeft e  = [EditLeft e]

editRight :: Edit b -> Edit (Either a b)
editRight [] = []
editRight e  = [EditRight e]

instance (Editable a, Editable b) => Editable (Either a b) where

    docCost (Left  a) = 1 + docCost a
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
    ePatch EditLeft{} Right{} = error "impossible"
    ePatch EditRight{} Left{} = error "impossible"

    diff (Left  a) (Left  a') = editLeft  $ diff a a'
    diff (Right b) (Right b') = editRight $ diff b b'
    diff _ x = [SetEither x]

    eMerge (Left  a) (EditLeft  ea) (EditLeft  ea') = editLeft  *** editLeft  $ merge a ea ea'
    eMerge (Right b) (EditRight eb) (EditRight eb') = editRight *** editRight $ merge b eb eb'
    eMerge d a b = secondWins d a b

    eInverse (Left  a) (EditLeft  e) = editLeft  $ inverse a e
    eInverse (Right b) (EditRight e) = editRight $ inverse b e
    eInverse x         (SetEither _) = [SetEither x]
    eInverse Left{} EditRight{} = error "impossible"
    eInverse Right{} EditLeft{} = error "impossible"

deriving instance (Show a, Show (EEdit a), Show b, Show (EEdit b)) => Show (EEdit (Either a b))

---------------------------------------- (Bounded, Enum) instance

newtype Atom a = Atom { unAtom :: a }
  deriving (Eq, Ord, Bounded, Enum)

instance Show a => Show (Atom a) where show = show . unAtom

instance (Eq a) => Editable (Atom a) where
    newtype EEdit (Atom a) = ReplaceEnum (Atom a)
      deriving (Show)

    eCost _ = 1
    docCost _ = 1

    diff a b = [ReplaceEnum b | a /= b]
    ePatch (ReplaceEnum a) _ = a
    eMerge _ ReplaceEnum{} b@ReplaceEnum{} = ([b], mempty)
    eInverse d ReplaceEnum{} = [ReplaceEnum d]

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
        -- FUTUREWORK: detect swapping/moving and duplication of items
        --  MoveItem Int Int     -- needed for expressing .....
        --  DuplicateItem Int

    eCost = \case
        InsertItem _ d -> 1 + docCost d
        DeleteItem _   -> 1
        EditItem _ p   -> 1 + cost p

    ePatch (DeleteItem i  ) xs = take i xs <> drop (i+1) xs
    ePatch (InsertItem i x) xs = take i xs <> (x: drop i xs)
    ePatch (EditItem   i x) xs = take i xs <> (patch x (xs !! i): drop (i+1) xs)

    diff s1 s2 = snd . snd $ triangle !! (length s1 + lenS2) !! lenS2
      where
        triangle = scanl ff [(error "impossible", mk [])] $ zip (revInits $ reverse s1) s2''

        s2' = reverse (zip [0..] s2) <> repeat (error "impossible")

        s2'' = tail $ scanl (flip add) (mk []) [mk [InsertItem n y] | ~(n, y) <- s2']

        ff es'@((_, e): es) (xs, ye)
            = (e, mk [DeleteItem lenS2] `add` e): zipWith4 gg xs s2' es' es <> [(error "impossible", ye)]
          where
            gg x (n, y) (fo, f1) (_, f2) = (,) f2 $ if null dxy then fo else
                minimumBy (compare `on` fst)
                    [ mk (editItem n dxy) `add` fo
                    , mk [InsertItem n y] `add` f1
                    , mk [DeleteItem n]   `add` f2
                    ]
              where
                dxy = diff x y
        ff [] _ = error "impossible"

        lenS2 = length s2

        mk x = (cost x, x)
        (ca, a) `add` (cb, b) = (ca + cb, a <> b)

        revInits :: [a] -> [[a]]
        revInits = f []
          where
            f acc xs = acc: f (head xs: acc) (tail xs)

    eMerge d (EditItem i x) (EditItem i' y) | i == i' = editItem i *** editItem i $ merge (d !! i) x y
    eMerge _ (EditItem i _) (DeleteItem i') | i == i' = ([DeleteItem i], [])      -- information lost!
    eMerge d (DeleteItem i) (EditItem i' x) | i == i' = (InsertItem i (d !! i): editItem i x, [])
    eMerge _ (DeleteItem i) (DeleteItem i') | i == i' = ([], [])
    eMerge _ a b = (modify 0 a b, modify 1 b a)
      where
        modify l e = \case
            InsertItem i x -> [InsertItem (di l i) x]
            DeleteItem i   -> [DeleteItem (di 1 i)  ]
            EditItem   i x -> [EditItem   (di 1 i) x]
          where
            di k i = case e of
                InsertItem j _ | j==i      -> i+k
                               | j<i       -> i+1
                               | otherwise -> i
                DeleteItem j   | j==i      -> i
                               | j<i       -> i-1
                               | otherwise -> i
                EditItem{}     -> i

    eInverse d = \case
        EditItem i x   -> editItem i $ inverse (d !! i) x
        DeleteItem i   -> [InsertItem i (d !! i)]
        InsertItem i _ -> [DeleteItem i]

deriving instance (Show a, Show (EEdit a)) => Show (EEdit [a])

---------------------------------------- StrictText instance

instance Editable Text.Text where
    newtype EEdit Text.Text = EText {unEText :: EEdit String}
        deriving (Show)
    docCost = Text.length
    eCost = eCost . unEText
    diff a b = f 0 $ Diff.diff (Text.unpack a) (Text.unpack b)
      where
        f !n (Diff.Both{}: es) = f (n+1) es
        f n (Diff.New c: es) = EText (InsertItem n c): f (n+1) es
        f n (Diff.Old{}: es) = EText (DeleteItem n): f n es
        f _ [] = []
    ePatch e = Text.pack . ePatch (unEText e) . Text.unpack
    eMerge d a b = map EText *** map EText $ eMerge (Text.unpack d) (unEText a) (unEText b)
    eInverse d = map EText . eInverse (Text.unpack d) . unEText

---------------------------------------- Set instance

editElem :: a -> Edit a -> Edit (Set.Set a)
editElem _ [] = []
editElem i p  = [EditElem i p]

instance (Editable a, Ord a) => Editable (Set.Set a) where

    docCost = (+1) . sum . map docCost . Set.elems

    data EEdit (Set.Set a)
        = DeleteElem a
        | InsertElem a          -- it is not valid to insert an elem which is already in the set
        | EditElem a (Edit a)   -- it is not valid to edit such that the result is already in the set

    eCost = \case
        InsertElem d -> 1 + docCost d
        DeleteElem _ -> 1
        EditElem _ e -> 1 + cost e

    ePatch (DeleteElem x) = Set.filter (/=x)
    ePatch (InsertElem x) = Set.insert x
    ePatch (EditElem x e) = Set.insert (patch e x) . Set.delete x

    diff a b = [DeleteElem x | x <- Set.elems $ Set.difference a b]
            <> [InsertElem x | x <- Set.elems $ Set.difference b a]

{-
    -- this is an 'eMerge' implementation for lists interpreted as sets.  for now, we just use
    -- 'secondWins'.

    eMerge _ a b | a == b = ([], [])
    eMerge d a@(EditElem i x) b@(EditElem i' y) | i == i'
        = if patch (x <> x2) i `notElem` unSet d
            then (editElem (patch x i) x2, editElem (patch y i) y2)
            else secondWins d a b
      where
        (x2, y2) = merge i x y
    eMerge d a@(EditElem i x) b@(EditElem i' y) | i /= i' && patch x i == patch y i' = secondWins d a b
    eMerge _ (EditElem i x) (DeleteElem i') | i == i' = ([DeleteElem (patch x i)], [])  -- information lost!
    eMerge _ (DeleteElem i) (EditElem i' x) | i == i' = ([InsertElem (patch x i)], [])
    eMerge _ (InsertElem x) (EditElem y e) | x == patch e y = if x == y then ([], []) else ([DeleteElem y], [])
    eMerge d a@(EditElem y e) b@(InsertElem x) | x == patch e y = if x == y then ([], []) else secondWins d a b
    eMerge _ a b = ([b], [a])
-}

    eInverse _ = \case
        EditElem x e -> [EditElem (patch e x) (inverse x e)]
        DeleteElem x -> [InsertElem x]
        InsertElem x -> [DeleteElem x]

deriving instance (Show a, Show (EEdit a)) => Show (EEdit (Set.Set a))

instance (Eq a, Editable a) => Eq (EEdit (Set.Set a)) where
    InsertElem a == InsertElem b = a == b
    DeleteElem a == DeleteElem b = a == b
    EditElem a ea == EditElem b eb = a == b && patch ea a == patch eb b
    _ == _ = False

---------------------------------------- Map instance

-- | FUTUREWORK: implement Editable Map
newtype Map a b = Map {unMap :: [(a, b)]}
   deriving (Show, Eq, Ord)

------------------------------------------------------- auxiliary definitions

class Representable a where
  type Rep a
  to   :: Rep a -> a
  from :: a -> Rep a
