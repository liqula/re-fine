{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}

-- | "Refine.Common.OT" for 'RawContent'.
--
-- We use 'NewDoc' as a representation of 'RawContent' based on finger trees
-- <http://hackage.haskell.org/package/fingertree-tf>.  This is a generalisation of sequences, which
-- give fast access to both ends of a list <http://hackage.haskell.org/package/sequence>, but uses
-- the 'Measure' type family for more powerful indexing.
module Refine.Common.VDoc.OT
  ( ChangeSet(ChangeSet)
  , unChangeSet
  , ChangeMap(ChangeMap)
  , unChangeMap

  , DocMeasure(..)
  , keyMeasure

  , idEdit
  , editStart
  , editEnd
  , reverseEdit
  , fullEdit
  , transformOldToNew
  , transformNewToOld
  , reposition

  , measureSeekStylePosition
  , measureSeekPosition
  , unSeekPosition
  , measurePosition
  , isValidNewDoc

  , transformRange
  , decorateEdit

  , fromRawContent
  , toRawContent
  , fromEditRawContent
  , transformRangeOTDoc
  , showEditAsRawContentWithMarks
  , showEditAsRawContent
  , docEditRanges
  , docRanges
  , hideUnchangedParts

    -- for testing only:
  , NewDoc
  , dotDotDotPngUri
  , split
  , measureRowColumn
  ) where

#include "import_common.hs"

import qualified Data.Algorithm.Patience as Diff
import           Data.FingerTree (FingerTree, Measured(..))
import           Data.Foldable
import           Data.List
import           Data.Maybe

import           Refine.Common.OT hiding (compressEdit)
import           Refine.Common.Types.Core hiding (Edit)


-- * fingertree amendments

-- FUTUREWORK: make this a patch to https://github.com/pavelchristof/fingertree-tf.

-- | interface for splittable sequences
--
-- The Monoid instance is used to join the sequences.
--
-- The elements of the sequence should be measurable.
-- The whole sequence should be measurable too and it should have the same type of measure as its elements.
class (Monoid a, Measured (SeqElem a), Measured a, Measure a ~ Measure (SeqElem a))
   => HasSplit a where

  -- | element type
  type SeqElem a

  -- | one-element container; empty and bigger sequences can be constracted with mempty and (<>)
  singleton :: SeqElem a -> a

  -- | 'Access a b' is the required connection
  -- between 'Measure a' and 'b' to be able to perform the split
  type Access a b

  -- | split the container at 'b'
  split :: Ord b => Access a b -> b -> a -> (a, a)

  -- | split one element from the left
  viewl :: a -> Maybe (SeqElem a, a)

  -- | split one element from the right
  viewr :: a -> Maybe (a, SeqElem a)

-- | empty container
pattern Empty :: HasSplit a => a
pattern Empty <- (viewl -> Nothing)
  where Empty = mempty

infixr 7 :<

-- | pattern synonym for viewl
pattern (:<) :: HasSplit a => SeqElem a -> a -> a
pattern a :< as <- (viewl -> Just (a, as))
  where a :< as = singleton a <> as

infixl 7 :>

-- | pattern synonym for viewl
pattern (:>) :: HasSplit a => a -> SeqElem a -> a
pattern as :> a <- (viewr -> Just (as, a))
  where as :> a = as <> singleton a


-- | FingerTree is splittable sequence
instance Measured a => HasSplit (FingerTree a) where

  type SeqElem (FingerTree a) = a

  singleton = FingerTree.singleton

  -- the access needed for the split is a function from 'Measure a' to @b@
  --
  -- the function must be monotonous in @b@
  type Access (FingerTree a) b = Measure a -> b

  split f b = FingerTree.split ((> b) . f)

  viewl x = case FingerTree.viewl x of
    FingerTree.EmptyL -> Nothing
    a FingerTree.:< as -> Just (a, as)

  viewr x = case FingerTree.viewr x of
    FingerTree.EmptyR -> Nothing
    as FingerTree.:> a -> Just (as, a)


-- | The measure of a strict text is its length.
instance Measured ST where
  type Measure ST = Sum Int  -- | Sum Int is a monoid with addition
  measure = Sum . ST.length

-- | The measure of a text element should have the same type as the text measure.
instance Measured Char where
  type Measure Char = Sum Int
  measure _ = Sum 1

data STAccess b where
  STAccessByIndex :: STAccess Int

-- 'ST' is a splittable sequence of 'Char's.
instance HasSplit ST where

  type SeqElem ST = Char

  -- We allow to split 'ST' only by exact integer indices for efficiency reasons.
  -- The only role of the 'Access' is to obtain a witness at use sites that
  -- @b@ is an @Int@.
  type Access ST b = STAccess b

  split STAccessByIndex i txt = (ST.take i txt, ST.drop i txt)

  viewl txt
    | ST.length txt == 0 = Nothing
    | otherwise = Just (ST.head txt, ST.tail txt)

  viewr txt
    | ST.length txt == 0 = Nothing
    | otherwise = Just (ST.init txt, ST.last txt)

  singleton = ST.singleton


-- | Call 'split' on 'snd' repeatedly and return the list of 'fst's.  Termination: if 'snd' is
-- empty, return empty list; if not, return at least the first 'splits' is run on its tail.
--
-- all elements in the result list's tail are non-empty
splits :: (HasSplit a, Ord b) => Access a b -> b -> a -> [a]
splits f b (split f b -> (d1, d2)) = d1: case d2 of
  Empty -> []
  x :< xs -> splits f b xs & Lens._head %~ (x :<)

-- | List of measures for all splits (accumulates).
splitMeasures :: (HasSplit a, Ord b) => Access a b -> b -> a -> [Measure a]
splitMeasures f b = scanl1 (<>) . map measure . splits f b

-- | Split a 'FingerTree' up into the given points
--
-- Example for @b@: 'Position'.  This function could also be used to e.g. split at all mark
-- beginnings.
splitsAt :: (HasSplit a, Ord b) => Access a b -> [b] -> a -> [a]
splitsAt f bs_ = reverse . g (reverse bs_)
  where
  g [] d = [d]
  g (b: bs) (split f b -> (d1, d2)) = d2: g bs d1

-- computes a function of the measure at a given split point
--
-- useful for conversion between different kind of positions
reposition :: (HasSplit a, Ord p1) => Access a p1 -> Getter (Measure a) p2 -> a -> p1 -> p2
reposition f1 f2 t p1 = measure (fst $ split f1 p1 t) ^. f2


-- * ChangeSet, ChangeMap

-- | Keep track of styling in documents.
-- Toggle @a@ in and out of the set with '(<>)'.
--
-- ChangeSets accumulate: a 'DocPiece' is styled with all styles in all ChangeSets associated with
-- 'DocPiece's to the left in this position.  (This is both more memory compact and faster: if you
-- delete a mark, you cut the doc at the pieces where the mark style changes and remove it there.
-- No other pieces of the doc need to be touched.)
newtype ChangeSet a = ChangeSet {_unChangeSet :: Set a}
  deriving (Eq, Ord, Show)

makeLenses ''ChangeSet

instance Ord a => Monoid (ChangeSet a) where
  mempty = ChangeSet mempty
  ChangeSet a `mappend` ChangeSet b
    | Set.null a = ChangeSet b     -- optimization for better sharing
    | Set.null b = ChangeSet a     -- optimization for better sharing
    | otherwise = ChangeSet $ Set.difference a b <> Set.difference b a

-- | Like 'ChangeSet', but for 'Measure'.  Counting the number of toggles is necessary here to make
-- the functions on measures monotonic.
--
-- The ChangeMap is also good to inspect what kind of styles are in a piece of document.
--
-- invariant: values are always > 0
newtype ChangeMap a = ChangeMap {_unChangeMap :: Map a Int}
  deriving (Eq, Ord, Show)

makeLenses ''ChangeMap

instance Ord a => Monoid (ChangeMap a) where
  mempty = ChangeMap mempty
  ChangeMap a `mappend` ChangeMap b
    | Map.null a = ChangeMap b     -- optimization for better sharing
    | Map.null b = ChangeMap a     -- optimization for better sharing
    | otherwise = ChangeMap $ Map.unionWith (+) a b

-- | @'toChangeSet' . 'toChangeMap' == 'id'@.  However, @'toChangeMap' . 'toChangeSet' /= 'id'@.
-- The map contains strictly more information, namely the number of previous toggles as opposed to
-- whether this number is even or odd.
toChangeSet :: Ord a => ChangeMap a -> ChangeSet a
toChangeSet = ChangeSet . Map.keysSet . Map.filter odd . _unChangeMap

-- | See 'toChangeMap'.
toChangeMap :: ChangeSet a -> ChangeMap a
toChangeMap = ChangeMap . Map.fromSet (const 1) . _unChangeSet


-- * SimpleEdit (not 'Doc'-specific)

-- | SimpleEdit stores the copied and deleted parts too, because it is intended
-- to represent an edit *plus* the original document
data SimpleEdit a
  = SECopy   a
  | SEDelete a
  | SEInsert a
  deriving (Eq, Ord, Show, Functor)

instance Measured a => Measured (SimpleEdit a) where
  -- | The measure has two part: the measure of the old document and the measure of the new document.
  -- Copied parts contribute to both measure.
  type Measure (SimpleEdit a) = (Measure a{- old -}, Measure a{- new -})
  measure = \case
    SECopy   a -> (x, x) where x = measure a
    SEDelete a -> (measure a, mempty)
    SEInsert a -> (mempty, measure a)

-- | SeqEdit is a sequence of simple edits.
newtype SeqEdit a = SeqEdit {unSeqEdit :: FingerTree (SimpleEdit a)}

-- | Usually the @a@s in SeqEdit are also sequences, and the Monoid instance of
-- SeqEdit takes care that pieces of the same tag (copy, delete or insert) are joined.
instance (Measured a, Monoid a) => Monoid (SeqEdit a) where
  mempty = SeqEdit mempty
  SeqEdit a `mappend` SeqEdit b = case (a, b) of
    (d1 :> SECopy   x, SECopy   y :< d2) -> SeqEdit $ d1 <> SECopy   (x <> y) :< d2
    (d1 :> SEDelete x, SEDelete y :< d2) -> SeqEdit $ d1 <> SEDelete (x <> y) :< d2
    (d1 :> SEInsert x, SEInsert y :< d2) -> SeqEdit $ d1 <> SEInsert (x <> y) :< d2
    _ -> SeqEdit $ a <> b

-- | The SeqEdit sequence has the same measure as its elements.
instance Measured a => Measured (SeqEdit a) where
  type Measure (SeqEdit a) = (Measure a{- old -}, Measure a{- new -})
  measure = measure . unSeqEdit

-- | Lens access is needed when we would like to know the measure for a given position.
-- This is important when we would like to have an ST as a part of a more complex sequence, for example.
type LensAccess a b = ALens' (Measure a) b

-- | Relative access also contains the offset measure,
-- which is the measure of all the elements before the current subsequence
type RelativeAccess a b = (Measure a, LensAccess a b)

-- | Edit sequences can be accessed in two distict ways.
-- Either the measure of the old sequence can be inpected, or the measure of the new sequence.
data SeqEditAccess a b where
  SeqEditAccessOld :: (Access a b ~ RelativeAccess a b) => LensAccess a b -> SeqEditAccess a b
  SeqEditAccessNew :: (Access a b ~ RelativeAccess a b) => LensAccess a b -> SeqEditAccess a b

-- | A sequence of edits is a sequence if its elements are sequences themselves.
instance (HasSplit a) => HasSplit (SeqEdit a) where

  type SeqElem (SeqEdit a) = SimpleEdit (SeqElem a)

  type Access (SeqEdit a) b = SeqEditAccess a b

  singleton = SeqEdit . singleton . fmap singleton

  split m b (SeqEdit d) = SeqEdit *** SeqEdit $ case split (mkFun m) b d of
      (d1, SECopy   (split (mkLens m $ measure d1) b -> (x1, x2)) :< d2)
        -> (joinToRight SECopy   x1 d1, joinToLeft SECopy   x2 d2)
      (d1, SEDelete (split (mkLens m $ measure d1) b -> (x1, x2)) :< d2)
        -> (joinToRight SEDelete x1 d1, joinToLeft SEDelete x2 d2)
      (d1, SEInsert (split (mkLens m $ measure d1) b -> (x1, x2)) :< d2)
        -> (joinToRight SEInsert x1 d1, joinToLeft SEInsert x2 d2)
      x -> x
     where
       mkFun :: Access (SeqEdit a) b -> Measure (SeqEdit a) -> b
       mkFun (SeqEditAccessOld f) = (^. cloneLens f) . fst
       mkFun (SeqEditAccessNew f) = (^. cloneLens f) . snd

       mkLens :: Access (SeqEdit a) b -> Measure (SeqEdit a) -> Access a b
       mkLens (SeqEditAccessOld f) (m', _) = (m', f)
       mkLens (SeqEditAccessNew f) (_, m') = (m', f)

  viewl (SeqEdit x) = case x of
    Empty -> Nothing
    SECopy   (b :< bs) :< as -> Just (SECopy   b, SeqEdit $ joinToLeft SECopy   bs as)
    SEDelete (b :< bs) :< as -> Just (SEDelete b, SeqEdit $ joinToLeft SEDelete bs as)
    SEInsert (b :< bs) :< as -> Just (SEInsert b, SeqEdit $ joinToLeft SEInsert bs as)

  viewr (SeqEdit x) = case x of
    Empty -> Nothing
    as :> SECopy   (bs :> b) -> Just (SeqEdit $ joinToRight SECopy   bs as, SECopy   b)
    as :> SEDelete (bs :> b) -> Just (SeqEdit $ joinToRight SEDelete bs as, SEDelete b)
    as :> SEInsert (bs :> b) -> Just (SeqEdit $ joinToRight SEInsert bs as, SEInsert b)

-- | helper function
joinToLeft :: (HasSplit m, HasSplit t) => (t -> SeqElem m) -> t -> m -> m
joinToLeft _ Empty b = b
joinToLeft f a b = f a :< b

-- | helper function
joinToRight :: (HasSplit m, HasSplit t) => (t -> SeqElem m) -> t -> m -> m
joinToRight _ Empty b = b
joinToRight f a b = b :> f a

-- | edit with same start and end object.
idEdit :: Measured a => a -> SeqEdit a
idEdit = SeqEdit . singleton . SECopy

-- | document before edit.  names are motivated by cat theory: this is the start of the edit arrow.
editStart :: Monoid a => SeqEdit a -> a
editStart = mapMaybeSequence $ \case
  SECopy   a -> Just a
  SEDelete a -> Just a
  SEInsert _ -> Nothing

-- | document after edit.
editEnd :: Monoid a => SeqEdit a -> a
editEnd = mapMaybeSequence $ \case
  SECopy   a -> Just a
  SEDelete _ -> Nothing
  SEInsert a -> Just a

-- | helper function
mapMaybeSequence :: Monoid b => (SimpleEdit a -> Maybe b) -> SeqEdit a -> b
mapMaybeSequence f = mconcat . mapMaybe f . toList . unSeqEdit

-- | reverse (undo) of an edit
reverseEdit :: Measured a => SeqEdit a -> SeqEdit a
reverseEdit = SeqEdit . FingerTree.fmap' f . unSeqEdit
  where
    f = \case
      SECopy   a -> SECopy   a
      SEDelete a -> SEInsert a
      SEInsert a -> SEDelete a

-- | keep deleted parts of the doc.  used when transforming ranges to diff view.
fullEdit :: (Measured a, Monoid a) => SeqEdit a -> SeqEdit a
fullEdit = foldMap (SeqEdit . singleton . f) . toList . unSeqEdit
  where
    f :: SimpleEdit a -> SimpleEdit a
    f = \case
      SECopy   a -> SECopy   a
      SEDelete a -> SECopy   a
      SEInsert a -> SEInsert a

-- transform old positions to new ones
transformOldToNew :: (HasSplit a, Ord b, Access a b ~ RelativeAccess a b)
  => LensAccess a b -> SeqEdit a -> b -> b
transformOldToNew f = reposition (SeqEditAccessOld f) (to snd . cloneLens f)

-- transform new positions to old ones
transformNewToOld :: (HasSplit a, Ord b, Access a b ~ RelativeAccess a b)
  => LensAccess a b -> SeqEdit a -> b -> b
transformNewToOld f = reposition (SeqEditAccessNew f) (to fst . cloneLens f)


-- * DocElem

type DocElem = DocElem_ ST

-- | A document element is either a non-empty text with the same styling or a boundary between blocks
data DocElem_ a
  = DocElem (ChangeSet EStyle) a            -- ^ consecutive chars in one 'Block' with the same styling.
  | NewBlock BlockKey BlockDepth BlockType  -- ^ boundary between 'Block's.  (like a newline, but with more info.)
  deriving (Eq, Ord, Show)

type EStyle = Either Entity Style

-- | Measure calculation for a document element
instance (Measured a, Measure a ~ Sum Int) => Measured (DocElem_ a) where
  type Measure (DocElem_ a) = DocMeasure
  measure = \case
    DocElem s c     -> DocMeasure mempty mempty (Split.M $ measure c) (toChangeMap s)
    NewBlock key _ _ -> DocMeasure (Sum 1) (Last $ Just key) Split.split mempty

-- | Composite 'Monoid' covering everything we need for navigating in a document.
data DocMeasure = DocMeasure
  { _rowMeasure   :: Sum Int                -- ^ row index + 1
  , _keyMeasure   :: Last BlockKey          -- ^ last blockkey
  , _charMeasure  :: Split.Split (Sum Int)  -- ^ column index + sum of non-newline chars before the last newline
  , _styleMeasure :: ChangeMap EStyle       -- ^ styles applicable to a given doc piece
  }

makeLenses ''DocMeasure

instance Monoid DocMeasure where
  mempty = DocMeasure mempty mempty mempty mempty
  DocMeasure a b c d `mappend` DocMeasure a' b' c' d' = DocMeasure (a <> a') (b <> b') (c <> c') (d <> d')

-- | The set of styles which are active at a position in a document.
styleSet :: DocMeasure -> ChangeSet EStyle
styleSet = toChangeSet . _styleMeasure

-- | The number of stylable positions before a position in a document. Usually used as an ordinal number.
newtype SeekStylePosition = SeekStylePosition {_unSeekStylePosition :: Int}
  deriving (Eq, Ord, Show)

makeLenses ''SeekStylePosition

measureSeekStylePosition :: Lens' DocMeasure SeekStylePosition
measureSeekStylePosition = charMeasure . lens (getSum . Split.unsplit) g . Lens.from unSeekStylePosition
  where
    g (Split.M _) i = Split.M $ Sum i
    g (Sum a Split.:| _) i = Sum a Split.:| Sum (i - a)

-- | The number of all different positions before a position in a document. Usually used as an ordinal number.
newtype SeekPosition = SeekPosition {_unSeekPosition :: Int}
  deriving (Eq, Ord, Show)

makeLenses ''SeekPosition

measureSeekPosition :: Lens' DocMeasure SeekPosition
measureSeekPosition = lens f g . Lens.from unSeekPosition
  where
    f (DocMeasure r _ c _) = getSum $ r <> Split.unsplit c

    g (DocMeasure r k (Split.M _) s) i = DocMeasure r k (Split.M $ Sum i) s
    g (DocMeasure (Sum r) k (Sum a Split.:| _) s) i
      = DocMeasure (Sum r) k (Sum a Split.:| Sum (i - r - a)) s

-- | row and column measure
measureRowColumn :: Lens' DocMeasure (Int, Int)
measureRowColumn = lens (\x -> (getSum $ x ^. rowMeasure, getSum . getSplitSnd $ x ^. charMeasure)) g
  where
    g m (r, c) = m & rowMeasure .~ Sum r & charMeasure %~ h
      where
        h (Split.M _) = Split.M $ Sum c
        h (a Split.:| _) = a Split.:| Sum c

-- | support for conventional positions
measurePosition :: DocMeasure -> Position
measurePosition (DocMeasure r k c _)
  = Position (BlockIndex (getSum r - 1) (fromMaybe (BlockKey "") $ getLast k)) (getSum $ getSplitSnd c)

-- | helper function
getSplitSnd :: Split.Split a -> a
getSplitSnd (Split.M x) = x
getSplitSnd (_ Split.:| x) = x


-- * NewDoc

-- | Representation of 'RawContent' better suited for dealing with 'Edit's and indexing.
-- Blocks are prefixed with @'\n'@ and appended: @["oumpf", "", "boo"] => "\noumpf\n\nboo"@.
newtype NewDoc = NewDoc {unNewDoc :: FingerTree DocElem}
  deriving (Eq, Show)

instance Measured NewDoc where
  type Measure NewDoc = DocMeasure
  measure = measure . unNewDoc

-- | The monoid instance of NewDoc takes care of joining elements with the same styling.
instance Monoid NewDoc where
  mempty = NewDoc mempty
  NewDoc (d1 :> DocElem s t) `mappend` NewDoc (DocElem (ChangeSet s') t' :< d2) | Set.null s'
    = NewDoc $ d1 <> DocElem s (t <> t') :< d2
  NewDoc x `mappend` NewDoc y = NewDoc $ x <> y

-- NewDoc is a splittable sequence of DocElems
instance HasSplit NewDoc where

  type SeqElem NewDoc = DocElem

  type Access NewDoc b = RelativeAccess NewDoc b

  split (base, f) b (NewDoc d) = NewDoc *** NewDoc $
    case split (\m -> (base <> m) ^. cloneLens f) b d of
      (d1, DocElem s t :< d2)
        | m <- base <> measure d1
        , m' <- m & cloneLens f .~ b
        , c <- ((-) `on` (^. measureRowColumn . _2)) m' m
        , c > 0
        , (t1, t2) <- split STAccessByIndex c t
        -> (d1 :> DocElem s t1, DocElem mempty t2 :< d2)
      x -> x

  viewl (NewDoc x) = case x of
    Empty -> Nothing
    DocElem s (t :< ts) :< as -> Just (DocElem s $ singleton t, NewDoc $ DocElem mempty ts :< as)
    a :< as -> Just (a, NewDoc as)

  viewr (NewDoc x) = case x of
    Empty -> Nothing
    as :> DocElem s (ts :> t) -> Just (NewDoc $ as :> DocElem s ts, DocElem mempty $ singleton t)
    as :> a -> Just (NewDoc as, a)

  singleton = NewDoc . singleton

-- | Check for non-emptiness.  (Styles are not toggled off if they apply to the last char in a doc.
-- This is the natural behavior given that closing a style range is done on the first char *after*
-- the range (which does not exist in this case), and has no known drawbacks.  use 'SyleSet' to
-- observe open styles at the very end of the doc.
--
-- FIXME: return 'False' if entity ranges overlap or elements with the same styling are not joined
isValidNewDoc :: NewDoc -> Bool
isValidNewDoc (NewBlock{} :< _) = True
isValidNewDoc _ = False

-- | Try to toggle one specific style for the whole document.
-- Sometimes it is not possible (if the the document contains no characters)
toggleStyle :: ChangeSet EStyle -> NewDoc -> NewDoc
toggleStyle sty d
  | Set.null $ _unChangeSet sty = d  -- this line is an optimization
  | (d1, d2) <- split (mempty, measureSeekStylePosition . unSeekStylePosition) 0 d
      = d1 <> toggleStyle_ sty d2

-- | More efficient variant of 'toggleStyle'. Use only if the first element cannot be 'NewBlock'.
toggleStyle_ :: ChangeSet EStyle -> NewDoc -> NewDoc
toggleStyle_ sty d
  | DocElem s c :< d2 <- d = DocElem (sty <> s) c :< d2
  | otherwise = d

-- Append two document and control what styles are opened, closed or left as they are between them.
mappendWithStyleChange :: (Set EStyle -> Set EStyle) -> NewDoc -> NewDoc -> NewDoc
mappendWithStyleChange fun d@(styleSet . measure -> c@(ChangeSet sty)) d' = d <> toggleStyle (c <> ChangeSet (fun sty)) d'

-- | More efficient variant of 'mappendWithStyleChange'.
-- Use only if the first element of the second document cannot be 'NewBlock'.
mappendWithStyleChange_ :: (Set EStyle -> Set EStyle) -> NewDoc -> NewDoc -> NewDoc
mappendWithStyleChange_ fun d@(styleSet . measure -> c@(ChangeSet sty)) d' = d <> toggleStyle_ (c <> ChangeSet (fun sty)) d'

-- Append two documents preventing any styling leak from the first one to the second one.
-- Probably this is what you need in general.
mappendNewDoc :: NewDoc -> NewDoc -> NewDoc
mappendNewDoc = mappendWithStyleChange $ const mempty

-- | used for e.g. filtering out all marks.
filterStyle :: (EStyle -> Bool) -> NewDoc -> NewDoc
filterStyle p = NewDoc . FingerTree.fmap' (\case
    DocElem (ChangeSet s) c -> DocElem (ChangeSet $ Set.filter p s) c
    x -> x) . unNewDoc

-- | all styles occurring anywhere in a 'NewDoc'.
docStyles :: NewDoc -> Set EStyle
docStyles = Map.keysSet . _unChangeMap . _styleMeasure . measure

-- | all ranges decorated with a given style.
getStyleRanges :: NewDoc -> EStyle -> Ranges SeekStylePosition
getStyleRanges doc sty
  = RangesInner [Range a b | [a, b] <- everyNth 2
                                     . map (^. measureSeekStylePosition)
                                     $ splitMeasures (mempty, styleMeasure . unChangeMap . at sty . Lens.non 0) 0 doc]
  where
    everyNth :: Int -> [a] -> [[a]]
    everyNth _ [] = []
    everyNth i xs = take i xs: everyNth i (drop i xs)

-- | styling of ranges; use only if there are no ranges of this style in the document
addStyleRanges :: EStyle -> Ranges SeekStylePosition -> NewDoc -> NewDoc
addStyleRanges s rs
  = mconcat
  . mapTail (toggleStyle . ChangeSet $ Set.singleton s)
  . splitsAt (mempty, measureSeekStylePosition) [p | Range a b <- unRanges rs, p <- [a, b]]
  where
  mapTail f (x: xs) = x: map f xs
  mapTail _ _ = error "impossible"

-- | Split up a 'NewDoc' into segments corresponding to 'Block's in 'RawContent'.
-- @docBlocks "\noumph\n\n\nboo" == ["\noumph", "\n", "\n", "\nboo"]@
docBlocks :: NewDoc -> [NewDoc]
docBlocks ndoc = case splits (mempty, measureRowColumn . _1) 0 ndoc of
  Empty: v -> v
  bad -> error $ "docBlocks: invalid input: " <> show bad


-- * NewDocEdit

-- | compressed sequence of basic edits
type NewDocEdit = SeqEdit NewDoc

-- | Decorate and edit with styles
--
-- FIXME: NewBlock change is not decorated
-- FIXME: style change decoration is limited (only the first segment is decorated)
decorateEdit :: NewDocEdit -> NewDoc
decorateEdit = foldl f Empty . toList . unSeqEdit
  where
  f :: NewDoc -> SimpleEdit NewDoc -> NewDoc
  f d = \case
    SECopy   a -> mappendWithStyleChange (Set.delete (Right StyleDeleted) . Set.delete (Right StyleAdded)) d a
    SEDelete a -> mappendWithStyleChange (Set.insert (Right StyleDeleted) . Set.delete (Right StyleAdded)) d a
    SEInsert a -> mappendWithStyleChange (Set.delete (Right StyleDeleted) . Set.insert (Right StyleAdded)) d a


-- * interfacing 'NewDoc' with other representations

fromRawContent :: RawContent -> NewDoc
fromRawContent (RawContent blocks entitymap)
  = foldl mappendNewDoc Empty . map fromBlock $ NEL.toList blocks
  where
    fromBlock :: Block EntityKey BlockKey -> NewDoc
    fromBlock (Block' txt entities styles btype depth key)
      =  NewBlock key (BlockDepth depth) btype
      :< foldr (uncurry addStyle')
               (case txt of Empty -> Empty; _ -> singleton $ DocElem mempty txt)
               (    (second Right <$> Set.toList styles)
                 <> [ (r, Left (entitymap IntMap.! _unEntityKey ek))
                    | (ek, r) <- Set.toList entities]
               )

    addStyle' :: Range Int -> EStyle -> NewDoc -> NewDoc
    addStyle' (Range x y) s (splitCol y -> (splitCol x -> (d1, d2), d3))
      = mappendWithStyleChange_ (Set.delete s) (mappendWithStyleChange_ (Set.insert s) d1 d2) d3

    splitCol :: Int -> NewDoc -> (NewDoc, NewDoc)
    splitCol = split (mempty, measureRowColumn . _2)

toRawContent :: NewDoc -> RawContent
toRawContent (docBlocks -> dls@(_:_))
  = mkRawContentInternal . NEL.fromList $ zipWith toBlock (scanl (<>) mempty $ styleSet . measure <$> dls) dls
  where
    toBlock :: ChangeSet EStyle -> NewDoc -> Block Entity BlockKey
    toBlock (ChangeSet sty_) (toList . unNewDoc -> NewBlock key (BlockDepth depth) btype: d)
      = Block'
          (mconcat [c | DocElem _ c <- d])
          (Set.fromList [(e, r) | (r, Left e) <- ss])
          (Set.fromList [(r, s) | (r, Right s) <- ss])
          btype
          depth
          key
      where
        (is, as) = unzip [(ST.length txt, s) | DocElem (ChangeSet s) txt <- d]
        is'      = scanl (+) 0 is
        (end: _) = reverse is'

        ss = mkRanges ((,) 0 <$> Set.toList sty_) $ zip is' as

        mkRanges acc [] =
              [(Range beg end, sty) | (beg, sty) <- acc, end > beg]
        mkRanges acc ((i, s): xs) =
              [(Range beg i, sty) | (beg, sty) <- acc, sty `Set.member` s, i > beg]
           <> mkRanges  (  [(beg, sty) | (beg, sty) <- acc, sty `Set.notMember` s]
                        <> [(i, sty) | sty <- Set.elems s, sty `notElem` fmap snd acc])
                        xs

-- | FIXME: remove when 'OTDoc' has been replaced by 'NewDoc'.
--
-- Forgets the edit value and re-computes the diff with a different algorithm (patience diff).  This
-- is weird, but actually leads to nicer diff views in some cases.  (Anyway it's temporary.)
fromEditRawContent :: RawContent -> Edit RawContent -> NewDocEdit
fromEditRawContent rc edit
  = foldMap (singleton . f) $ Diff.diff (docPieces $ fromRawContent rc) (docPieces . fromRawContent $ patch edit rc)
  where
    f (Diff.Both c _) = SECopy   c
    f (Diff.New  c  ) = SEInsert c
    f (Diff.Old  c  ) = SEDelete c

    docPieces :: NewDoc -> [DocElem]
    docPieces = concatMap g . toList . unNewDoc
      where
        g = \case
          DocElem s (cs -> x: xs)
            -> DocElem s (cs [x :: Char]): (DocElem mempty . cs . (:[]) <$> xs)
          x -> [x]


-- * 'OTDoc'

-- transforms a style position range through an edit
transformRange :: NewDocEdit -> Range SeekStylePosition -> Range SeekStylePosition
transformRange edit = fmap $ transformOldToNew measureSeekStylePosition edit

-- tranformation of style position ranges through an edit
-- FIXME: change to
--    transformRangeRC :: Edit RawContent -> RawContent -> Range SeekStylePosition -> Range SeekStylPosition
transformRangeOTDoc :: Edit OTDoc -> OTDoc -> Range Position -> Range Position
transformRangeOTDoc edit doc
  = styleRangeToRange (editEnd e)
  . transformRange e
  . fmap (reposition (mempty, measureRowColumn) measureSeekStylePosition (editStart e) . mkPos)
  where
    e = fromEditRawContent (docToRawContent doc) (coerce [edit])

    mkPos :: Position -> (Int, Int)
    mkPos p = (p ^. rowIndex + 1, p ^. columnIndex)

-- computes the smallest range witch has the same style positions
styleRangeToRange :: NewDoc -> Range SeekStylePosition -> Range Position
styleRangeToRange d (Range x y)
  | x' <= y'  = RangeInner x' y'
  | otherwise = RangeInner x' x'  -- needed because this may happen: "xy" "\n" [ ] "abc"  --> "xy" ] "\n" [ "abc"
  where
    x' = reposition (mempty, measureSeekStylePosition) (to measurePosition) d x
    y' = measurePosition . measure . dropNewBlocks . fst $ split (mempty, measureSeekStylePosition) y d

    dropNewBlocks (d' :> NewBlock{}) = dropNewBlocks d'
    dropNewBlocks d' = d'

-- | show and edit as decorated RawContent, taking care of marks
showEditAsRawContentWithMarks :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContentWithMarks edit (fromRawContent -> doc)
  = toRawContent $ foldr (uncurry addStyleRanges) (decorateEdit dedit) transformedMarks
  where
    dedit = fromEditRawContent (toRawContent $ filterStyle (not . isMark) doc) edit

    transformedMarks =
      [ (s, RangesInner $ transformEditRange <$> unRanges (getStyleRanges doc s))
      | s <- Set.toList $ docStyles doc
      , isMark s
      ]

    transformEditRange = transformRange . {-compressEdit . -} fullEdit $ dedit

    isMark = either (const False) (has _Mark)

-- | this variant do not take care of marks
showEditAsRawContent :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContent edit doc
  = toRawContent . decorateEdit $ fromEditRawContent doc edit

-- | gives back the ranges where the old document was modified
docEditRanges :: Edit RawContent -> RawContent -> Ranges Position
docEditRanges edits rc
    = mconcat
    . map snd
    . docRanges True elemLength (\x -> [() | elemChanged x])
    $ showEditAsRawContent edits rc
  where
    elemLength :: LineElem -> Int
    elemLength ((_, ss), NonEmptyST txt)
        | Atom StyleAdded `Set.member` ss = 0
        | otherwise = ST.length txt

-- | helper function
elemChanged :: LineElem -> Bool
elemChanged ((_, ss), _)
    =  Atom StyleAdded   `Set.member` ss
    || Atom StyleDeleted `Set.member` ss
    || Atom StyleChanged `Set.member` ss

docRanges :: forall a . Ord a => Bool -> (LineElem -> Int) -> (LineElem -> [a]) -> RawContent -> [(a, Ranges Position)]
docRanges allowemptyranges elemlength includeelem rc
    = map mkRanges
    . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    . concat . zipWith blockRanges [(0::Int)..]
    . NEL.toList
    $ rawContentToDoc rc
  where
    blockRanges :: Int -> DocBlock -> [(a, Range Position)]
    blockRanges r (DocBlock _ _ key elems) = concat $ zipWith3 elemRanges offs (tail offs) elems
      where
        bi = BlockIndex r key

        offs :: [Int]
        offs = scanl (+) 0 $ elemlength <$> elems

        elemRanges :: Int -> Int -> LineElem -> [(a, Range Position)]
        elemRanges beg end le =
            [ (a, RangeInner x y) | a <- includeelem le]
          where
            x = Position bi beg
            y = Position bi end

    mkRanges xs@((a, _): _) = (a, mconcat $ rangesFromRange allowemptyranges . snd <$> xs)
    mkRanges _ = error "impossible"

dotDotDotPngUri :: ST
dotDotDotPngUri = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAUCAAAAAC/wNIYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QA/4ePzL8AAAAHdElNRQfhBxUGEycEQ5FxAAAAMklEQVQoz2P8z4APMDGMSpMuzcLAwPDhBAODAwdWBsP///+PMzAwPP6PlUGZ3YyDN0IBgS0cFJRLTz8AAAAldEVYdGRhdGU6Y3JlYXRlADIwMTctMDctMjFUMDY6MTk6MzkrMDI6MDADqkr+AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE3LTA3LTIxVDA2OjE5OjM5KzAyOjAwcvfyQgAAAABJRU5ErkJggg=="

-- | Take a document in diff mode, a number of blocks to keep preceeding and succeeding each changed
-- block, resp., and returns with all sequences of blocks far away from any change replaced by a
-- single block reading `...`.
hideUnchangedParts :: RawContent -> Int -> Int -> RawContent
hideUnchangedParts (NEL.toList . rawContentToDoc -> doc) blocksbefore blocksafter
    = docToRawContent
    . fromList_
    . concatMap showRegion
    . groupBy ((==) `on` fst)
    $ zip displayedLines doc
  where
    -- pattern matching makes for better ghcjs errors than 'error' calls.
    fromList_ l@(_:_) = NEL.fromList l

    showRegion :: [(Bool, DocBlock)] -> [DocBlock]
    showRegion bs@((True, _): _) = snd <$> bs
    showRegion ((False, blockKeyFromDocBlock -> bk): _) = [dotDotDot (bk <> BlockKey "_...")]
    showRegion _ = error "impossible"

    blockKeyFromDocBlock = unNonEditable . snd . fst :: DocBlock -> BlockKey

    dotDotDot bk = DocBlock NormalText (BlockDepth 0) bk [((Atom . Just $ EntityImage dotDotDotPngUri, mempty), "...")]

    {- example run
    blocksbefore = 2
    blocksafter  = 1
    changedLines =  -#----##

                    -
                    -#
                    -#-         # |
                    -#--        # #
                     #---       # |
                      ----      | |
                       ---#     # |
                        --##    # |
                         -##    # #
                          ##    # #
                           #
    -}
    displayedLines :: [Bool]
    displayedLines
        = drop (blocksbefore + 1)
        $ or <$> (take window (inits changedLines) <> (take window <$> tails changedLines))
      where
        window = blocksbefore + 1 + blocksafter

    changedLines :: [Bool]
    changedLines = lineChanged <$> doc

    lineChanged :: DocBlock -> Bool
    lineChanged (DocBlock _ _ _ elems) = any elemChanged elems
