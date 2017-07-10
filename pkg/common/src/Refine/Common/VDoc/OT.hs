{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}

-- | "Refine.Common.OT" for 'RawContent'.
--
-- We use 'NewDoc' as a representation of 'RawContent' based on finger trees
-- <http://hackage.haskell.org/package/fingertree-tf>.  This is a generalisation of sequences, which
-- give fast access to both ends of a list <http://hackage.haskell.org/package/sequence>, but uses
-- the 'Measure' type family for more powerful indexing.
module Refine.Common.VDoc.OT
  ( ChangeSet(ChangeSet)
  , ChangeMap(ChangeMap)
  , DocMeasure(..)

  , idEdit
  , editStart
  , editEnd
  , reverseEdit
  , fullEdit
  , transformNewToOld
  , transformOldToNew
  , reposition

  , measureSeekStylePosition
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

  , unChangeSet
  , keyMeasure
  ) where

import qualified Data.Algorithm.Patience as Diff
import           Data.FingerTree (FingerTree, Measured(..))
import qualified Data.FingerTree as FingerTree
import           Data.Foldable
import qualified Data.IntMap as IntMap
import           Data.List
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Monoid.Split as Split
import qualified Data.Set as Set
import qualified Data.Text as ST
import           Control.Lens hiding ((:<), (:>))

import           Refine.Common.OT hiding (compressEdit)
import           Refine.Common.Prelude hiding (fromList, from)
import           Refine.Common.Types.Core hiding (Edit)


-- * fingertree amendments

-- FUTUREWORK: make this a patch to https://github.com/pavelchristof/fingertree-tf.

type MeasureLens a b = Lens' (Measure a) b

class (Monoid a, Measured (FTElem a), Measured a, Measure a ~ Measure (FTElem a)) => FT a where

  type FTElem a

  splitAtBy :: Ord b => MeasureLens a b -> b -> a -> (a, a)
  viewl :: a -> ViewL a
  viewr :: a -> ViewR a
  singleton :: FTElem a -> a

data ViewL a
  = EmptyL
  | FTElem a :< a

data ViewR a
  = EmptyR
  | a :> FTElem a

instance Measured a => FT (FingerTree a) where

  type FTElem (FingerTree a) = a

  splitAtBy f b = FingerTree.split ((> b) . (^. f))

  viewl x = case FingerTree.viewl x of
    FingerTree.EmptyL -> EmptyL
    a FingerTree.:< as -> a :< as

  viewr x = case FingerTree.viewr x of
    FingerTree.EmptyR -> EmptyR
    as FingerTree.:> a -> as :> a

  singleton = FingerTree.singleton

-- | Call 'split' on 'snd' repeatedly and return the list of 'fst's.  Termination: if 'snd' is
-- empty, return empty list; if not, return at least the first 'splits' is run on its tail.
--
-- FIXME: if the boolean function on measure does not return 'True' on the first element of some
-- intermediate 'FingerTree', the output is
splits :: (FT a, Ord b) => MeasureLens a b -> b -> a -> [a]
splits f b (splitAtBy f b -> (d1, d2)) = d1: case viewl d2 of
  EmptyL -> []
  x :< xs -> splits f b xs & _head %~ (singleton x <>)

-- | List of measures for all splits (accumulates).
splitMeasures :: (FT a, Ord b) => MeasureLens a b -> b -> a -> [Measure a]
splitMeasures f b = scanl1 (<>) . map measure . splits f b

-- | Take a @b@-valued function on measures (like for 'split') and a list of values of that
-- function, and split a 'FingerTree' up into the points where those values are reached.  Function
-- must be monotonous in @b@.
--
-- Example for @b@: 'Position'.  This function could also be used to e.g. split at all mark
-- beginnings.
splitsAt :: (FT a, Ord b) => MeasureLens a b -> [b] -> a -> [a]
splitsAt f bs_ = reverse . g (reverse bs_)
  where
  g [] d = [d]
  g (b: bs) (splitAtBy f b -> (d1, d2)) = d2: g bs d1

measureAt :: (FT a, Ord b) => MeasureLens a b -> b -> a -> Measure a
measureAt f b = measure . fst . splitAtBy f b

reposition :: (FT a, Ord b1) => MeasureLens a b1 -> Getter (Measure a) b2 -> a -> b1 -> b2
reposition f1 f2 t b1 = measureAt f1 b1 t ^. f2


-- * ChangeSet, ChangeMap

-- | Keep track of styling in 'FingerTree's.  This is associated with every element in the finger
-- tree.  Toggle @a@ in and out of the set with '(<>)'.
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


-- * EditPiece (not 'Doc'-specific)

data EditPiece a
  = EPCopy   a
  | EPDelete a
  | EPInsert a
  deriving (Eq, Ord, Show, Functor)

instance Measured a => Measured (EditPiece a) where
  type Measure (EditPiece a) = (Measure a{- old -}, Measure a{- new -})
  measure = \case
    EPCopy a   -> (x, x) where x = measure a
    EPDelete a -> (measure a, mempty)
    EPInsert a -> (mempty, measure a)

newtype FTEdit a = FTEdit {unFTEdit :: FingerTree (EditPiece a)}

instance (Measured a, Monoid a) => Monoid (FTEdit a) where
  mempty = FTEdit mempty
  FTEdit a `mappend` FTEdit b = case (viewr a, viewl b) of
    (d1 :> EPCopy   x, EPCopy   y :< d2) -> FTEdit $ d1 <> singleton (EPCopy   $ x <> y) <> d2
    (d1 :> EPDelete x, EPDelete y :< d2) -> FTEdit $ d1 <> singleton (EPDelete $ x <> y) <> d2
    (d1 :> EPInsert x, EPInsert y :< d2) -> FTEdit $ d1 <> singleton (EPInsert $ x <> y) <> d2
    _ -> FTEdit $ a <> b

instance Measured a => Measured (FTEdit a) where
  type Measure (FTEdit a) = (Measure a, Measure a)
  measure = measure . unFTEdit

instance FT a => FT (FTEdit a) where
  type FTElem (FTEdit a) = EditPiece (FTElem a)

  splitAtBy k b (FTEdit d) = f . (FTEdit *** FTEdit) $ splitAtBy k b d
    where
      -- TUNING: speed this up
      f (d1, viewl -> x :< d2)
        | not $ b < (measure d1 <> measure x) ^. k = f (d1 <> singleton x, d2)
      f x = x

  viewl (FTEdit x) = case viewl x of
    EmptyL -> EmptyL
    EPCopy   (viewl -> b :< bs) :< as -> EPCopy   b :< FTEdit (add EPCopy   bs as)
    EPDelete (viewl -> b :< bs) :< as -> EPDelete b :< FTEdit (add EPDelete bs as)
    EPInsert (viewl -> b :< bs) :< as -> EPInsert b :< FTEdit (add EPInsert bs as)
    where
      add f a b = case viewl a of
        EmptyL -> b
        _ -> singleton (f a) <> b

  viewr (FTEdit x) = case viewr x of
    EmptyR -> EmptyR
    as :> EPCopy   (viewr -> bs :> b) -> FTEdit (add EPCopy   bs as) :> EPCopy   b
    as :> EPDelete (viewr -> bs :> b) -> FTEdit (add EPDelete bs as) :> EPDelete b
    as :> EPInsert (viewr -> bs :> b) -> FTEdit (add EPInsert bs as) :> EPInsert b
    where
      add f a b = case viewr a of
        EmptyR -> b
        _ -> b <> singleton (f a)

  singleton = FTEdit . singleton . fmap singleton

-- | edit with same start and end object.
idEdit :: Measured a => a -> FTEdit a
idEdit = FTEdit . FingerTree.singleton . EPCopy

mapMaybeFT :: Monoid b => (EditPiece a -> Maybe b) -> FTEdit a -> b
mapMaybeFT f = mconcat . mapMaybe f . toList . unFTEdit

-- | document before edit.  names are motivated by cat theory: this is the start of the edit arrow.
editStart :: Monoid a => FTEdit a -> a
editStart = mapMaybeFT $ \case
  EPCopy   a -> Just a
  EPDelete a -> Just a
  EPInsert _ -> Nothing

-- | document after edit.
editEnd :: Monoid a => FTEdit a -> a
editEnd = mapMaybeFT $ \case
  EPCopy   a -> Just a
  EPDelete _ -> Nothing
  EPInsert a -> Just a

reverseEdit :: Measured a => FTEdit a -> FTEdit a
reverseEdit = FTEdit . FingerTree.fmap' f . unFTEdit
  where
    f = \case
      EPCopy   a -> EPCopy   a
      EPDelete a -> EPInsert a
      EPInsert a -> EPDelete a

-- | keep deleted parts of the doc.  used when transforming ranges to diff view.
fullEdit :: (Measured a, Monoid a) => FTEdit a -> FTEdit a
fullEdit = foldMap (FTEdit . singleton . f) . toList . unFTEdit
  where
    f :: EditPiece a -> EditPiece a
    f = \case
      EPCopy   a -> EPCopy   a
      EPDelete a -> EPCopy   a
      EPInsert a -> EPInsert a

transformOldToNew :: (FT a, Ord b) => MeasureLens a b -> FTEdit a -> b -> b
transformOldToNew f = reposition (_1 . f) (_2 . f)

transformNewToOld :: (FT a, Ord b) => MeasureLens a b -> FTEdit a -> b -> b
transformNewToOld f = reposition (_2 . f) (_1 . f)


-- * DocPiece

-- | TUNING: use 'NonEmptyST' (or @Either Char NonEmptyST@?  probably just 'NonEmptyST'...)  This is
-- probably important.
type DocPiece = DocPiece_ ST

data DocPiece_ a
  = DocPiece (ChangeSet EStyle) a           -- ^ consecutive chars in one 'Block' with the same styling.
  | NewBlock BlockKey BlockDepth BlockType  -- ^ boundary between 'Block's.  (like a newline, but with more info.)
  deriving (Eq, Ord, Show)

type EStyle = Either Entity Style

instance (Measured a, Measure a ~ Sum Int) => Measured (DocPiece_ a) where
  type Measure (DocPiece_ a) = DocMeasure
  measure = \case
    DocPiece s c     -> DocMeasure mempty mempty (Split.M $ measure c) (toChangeMap s)
    NewBlock key _ _ -> DocMeasure (Sum 1) (Last $ Just key) Split.split mempty

instance Measured Char where
  type Measure Char = Sum Int
  measure _ = Sum 1

-- | See comment for 'DocPiece'.  We replace this by the corresponding instance for 'NonEmptyST'.
instance Measured ST where
  type Measure ST = Sum Int
  measure = Sum . ST.length

-- | Composite 'Monoid' covering everything we need for navigating a 'NewDoc'.
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

-- | 'ChangeMap' from 'DocMeasure', translated into a 'ChangeSet'.
styleSet :: DocMeasure -> ChangeSet EStyle
styleSet = toChangeSet . _styleMeasure

newtype SeekStylePosition = SeekStylePosition {_unSeekStylePosition :: Int}
  deriving (Eq, Ord, Show)

makeLenses ''SeekStylePosition

measureSeekStylePosition :: Lens' DocMeasure SeekStylePosition
measureSeekStylePosition = charMeasure . lens (getSum . Split.unsplit) g . from unSeekStylePosition
  where
    g (Split.M _) i = Split.M $ Sum i
    g (Sum a Split.:| _) i = Sum a Split.:| Sum (i - a)

measurePosition :: DocMeasure -> Position
measurePosition (DocMeasure r k c _)
  = Position (BlockIndex (getSum r - 1) (fromMaybe (BlockKey "") $ getLast k)) (getSum $ getCol c)

getCol :: Split.Split a -> a
getCol (Split.M x) = x
getCol (_ Split.:| x) = x

measurePosition' :: Lens' DocMeasure (Int, Int)
measurePosition' = lens (\x -> (getSum $ x ^. rowMeasure, getSum . getCol $ x ^. charMeasure)) g
  where
    g m (r, c) = m & rowMeasure .~ Sum r & charMeasure %~ h
      where
        h (Split.M _) = Split.M $ Sum c
        h (a Split.:| _) = a Split.:| Sum c


-- * NewDoc

-- | Representation of 'RawContent' better suited for dealing with 'Edit's.  Blocks are prefixed
-- with @'\n'@ and appended: @["oumpf", "", "boo"] => "\noumpf\n\nboo"@.
newtype NewDoc = NewDoc {unNewDoc :: FingerTree DocPiece}
  deriving (Eq, Show)

instance Measured NewDoc where
  type Measure NewDoc = DocMeasure
  measure = measure . unNewDoc

instance Monoid NewDoc where
  mempty = NewDoc mempty
  NewDoc x `mappend` NewDoc y = case (viewr x, viewl y) of
    (d1 :> DocPiece s t, DocPiece (ChangeSet (Set.null -> True)) t' :< d2)
      -> NewDoc $ d1 <> singleton (DocPiece s $ t <> t') <> d2
    _ -> NewDoc $ x <> y

instance FT NewDoc where
  type FTElem NewDoc = DocPiece

  splitAtBy f b (NewDoc d) = case splitAtBy f b d of
    (d1, viewl -> DocPiece s t :< d2)
       | c > 0 -> ( NewDoc $ d1 <> singleton (DocPiece s $ ST.take c t)
                  , NewDoc $ singleton (DocPiece mempty $ ST.drop c t) <> d2
                  )
        where
          m = measure d1
          m' = m & f .~ b
          c = (m' ^. measurePosition' . _2) - (m ^. measurePosition' . _2)
    x -> NewDoc *** NewDoc $ x

  viewl (NewDoc x) = case viewl x of
    EmptyL -> EmptyL
    DocPiece s txt@((> 1) . ST.length -> True) :< as
      -> DocPiece s (ST.take 1 txt) :< NewDoc (singleton (DocPiece mempty $ ST.drop 1 txt) <> as)
    a :< as -> a :< NewDoc as

  viewr (NewDoc x) = case viewr x of
    EmptyR -> EmptyR
    as :> DocPiece s txt@((> 1) . ST.length -> True)
      -> NewDoc (as <> singleton (DocPiece s $ ST.take (i-1) txt)) :> DocPiece mempty (ST.drop (i-1) txt)
      where
        i = ST.length txt
    as :> a -> NewDoc as :> a

  singleton = NewDoc . singleton

unNewDoc' :: NewDoc -> [DocPiece]
unNewDoc' = concatMap f . toList . unNewDoc
  where
    f = \case
      DocPiece s (cs -> x: xs) -> DocPiece s (cs [x :: Char]): (DocPiece mempty . cs . (:[]) <$> xs)
      x -> [x]

-- | Check for non-emptiness.  (Styles are not toggled off if they apply to the last char in a doc.
-- This is the natural behavior given that closing a style range is done on the first char *after*
-- the range (which does not exist in this case), and has no known drawbacks.  use 'syleSet' to
-- observe open styles at the very end of the doc.
--
-- FIXME: return 'False' if style ranges overlap.  (did we forget other validity criteria?)
isValidNewDoc :: NewDoc -> Bool
isValidNewDoc (viewl -> NewBlock{} :< _) = True
isValidNewDoc _ = False

toggleStyle :: ChangeSet EStyle -> NewDoc -> NewDoc
toggleStyle sty d
  | Set.null $ _unChangeSet sty = d  -- this line is an optimization
  | (d1, viewl -> DocPiece s c :< d2) <- splitAtBy (measureSeekStylePosition . unSeekStylePosition) 0 d
      = d1 <> singleton (DocPiece (sty <> s) c) <> d2
  | otherwise = d

mappendWithStyleChange :: (Set EStyle -> Set EStyle) -> NewDoc -> NewDoc -> NewDoc
mappendWithStyleChange fun d@(styleSet . measure -> c@(ChangeSet sty)) d' = d <> toggleStyle (c <> ChangeSet (fun sty)) d'

mappendNewDoc :: NewDoc -> NewDoc -> NewDoc
mappendNewDoc = mappendWithStyleChange $ const mempty

-- | used for e.g. filtering out all marks.
filterStyle :: (EStyle -> Bool) -> NewDoc -> NewDoc
filterStyle p = NewDoc . FingerTree.fmap' (\case
    DocPiece (ChangeSet s) c -> DocPiece (ChangeSet $ Set.filter p s) c
    x -> x) . unNewDoc

-- | all styles occurring anywhere in a 'NewDoc'.
docStyles :: NewDoc -> Set EStyle
docStyles = Map.keysSet . _unChangeMap . _styleMeasure . measure

-- | all ranges decorated with a given style.
getStyleRanges :: NewDoc -> EStyle -> Ranges SeekStylePosition
getStyleRanges doc sty
  = RangesInner [Range a b | [a, b] <- everyNth 2 . map (^. measureSeekStylePosition) $ splitMeasures (styleMeasure . unChangeMap . at sty . non 0) 0 doc]
  where
    everyNth :: Int -> [a] -> [[a]]
    everyNth _ [] = []
    everyNth i xs = take i xs: everyNth i (drop i xs)

addStyleRanges :: EStyle -> Ranges SeekStylePosition -> NewDoc -> NewDoc
addStyleRanges s rs
  = mconcat
  . mapTail (toggleStyle . ChangeSet $ Set.singleton s)
  . splitsAt measureSeekStylePosition [p | Range a b <- unRanges rs, p <- [a, b]]
  where
  mapTail f (x: xs) = x: map f xs
  mapTail _ _ = error "impossible"

-- | Split up a 'NewDoc' into segments corresponding to 'Block's in 'RawContent'.
-- @docBlocks "\noumph\n\n\nboo" == ["\noumph", "\n", "\n", "\nboo"]@
docBlocks :: NewDoc -> [NewDoc]
docBlocks ndoc = case splits (measurePosition' . _1) 0 ndoc of
  (viewl -> EmptyL): v -> v
  bad -> error $ "docBlocks: invalid input: " <> show bad


-- * NewDocEdit

-- | TUNING: @newtype NewDocEdit = NewDocEdit (FTEdit NewDoc)@, then all edits would always be
-- compressed implicitly inside custom 'split' and 'mappend'.
type NewDocEdit = FTEdit NewDoc

decorateEdit :: NewDocEdit -> NewDoc
decorateEdit = foldl f mempty . toList . unFTEdit
  where
  f :: NewDoc -> EditPiece NewDoc -> NewDoc
  f d = \case
    EPCopy   a -> mappendWithStyleChange (Set.delete (Right StyleDeleted) . Set.delete (Right StyleAdded)) d a
    EPDelete a -> mappendWithStyleChange (Set.insert (Right StyleDeleted) . Set.delete (Right StyleAdded)) d a
    EPInsert a -> mappendWithStyleChange (Set.delete (Right StyleDeleted) . Set.insert (Right StyleAdded)) d a


-- * interfacing 'NewDoc' with other representations

fromRawContent :: RawContent -> NewDoc
fromRawContent (RawContent blocks entitymap) = foldl mappendNewDoc mempty . map fromBlock $ NEL.toList blocks
  where
    fromBlock :: Block EntityKey BlockKey -> NewDoc
    fromBlock (Block' txt entities styles btype depth key)
      =  singleton (NewBlock key (BlockDepth depth) btype)
      <> foldr (uncurry addStyle')
               (if ST.null txt then mempty else singleton $ DocPiece mempty txt)
               (    (second Right <$> Set.toList styles)
                 <> [ (r, Left (entitymap IntMap.! _unEntityKey ek))
                    | (ek, r) <- Set.toList entities]
               )

    addStyle' :: Range Int -> EStyle -> NewDoc -> NewDoc
    addStyle' (Range x y) s (splitCol y -> (splitCol x -> (d1, d2), d3))
      = mappendWithStyleChange (Set.delete s) (mappendWithStyleChange (Set.insert s) d1 d2) d3

    splitCol :: Int -> NewDoc -> (NewDoc, NewDoc)
    splitCol = splitAtBy (measurePosition' . _2)

toRawContent :: NewDoc -> RawContent
toRawContent (docBlocks -> dls)
  = mkRawContentInternal . NEL.fromList $ zipWith toBlock (scanl (<>) mempty $ styleSet . measure <$> dls) dls
  where
    toBlock :: ChangeSet EStyle -> NewDoc -> Block Entity BlockKey
    toBlock css@(ChangeSet sty_) nd@(toList . unNewDoc -> NewBlock key (BlockDepth depth) btype: d)
      = Block'
          (mconcat [c | DocPiece _ c <- d])
          (Set.fromList [(e, r) | (r, Left e) <- ss])
          (Set.fromList [(r, s) | (r, Right s) <- ss])
          btype
          depth
          key
      where
        (is, as) = unzip [(ST.length txt, s) | DocPiece (ChangeSet s) txt <- d]

        ss = mkRanges ((,) 0 <$> Set.toList sty_) . zip (scanl (+) 0 is)
           $ as <> [_unChangeSet $ css <> styleSet (measure nd)]

        mkRanges _ [] = []
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
  = foldMap (singleton . f) $ Diff.diff (unNewDoc' $ fromRawContent rc) (unNewDoc' . fromRawContent $ patch edit rc)
  where
    f (Diff.Both c _) = EPCopy   c
    f (Diff.New  c  ) = EPInsert c
    f (Diff.Old  c  ) = EPDelete c


-- * 'OTDoc'

transformRange :: NewDocEdit -> Range SeekStylePosition -> Range SeekStylePosition
transformRange edit r = transformOldToNew measureSeekStylePosition edit <$> r

-- tranformation of style position ranges through an edit
-- FIXME: change to
--    transformRangeRC :: Edit RawContent -> RawContent -> Range SeekStylePosition -> Range SeekStylPosition
transformRangeOTDoc :: Edit OTDoc -> OTDoc -> Range Position -> Range Position
transformRangeOTDoc edit doc
  = styleRangeToRange (editEnd e)
  . transformRange e
  . fmap (reposition measurePosition' measureSeekStylePosition $ editStart e)
  . fmap mkPos
  where
    e = fromEditRawContent (docToRawContent doc) (coerce [edit])

    mkPos :: Position -> (Int, Int)
    mkPos p = (p ^. rowIndex + 1, p ^. columnIndex)

styleRangeToRange :: NewDoc -> Range SeekStylePosition -> Range Position
styleRangeToRange d (Range x y)
  | x' <= y'  = RangeInner x' y'
  | otherwise = RangeInner x' x'
  where
    x' = reposition measureSeekStylePosition (to measurePosition) d x
    y' = measurePosition . measure . dropLineEnds . fst $ splitAtBy measureSeekStylePosition y d

    dropLineEnds (viewr -> d' :> NewBlock{}) = dropLineEnds d'
    dropLineEnds d' = d'

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


showEditAsRawContent :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContent edit doc
  = toRawContent . decorateEdit $ fromEditRawContent doc edit


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

elemChanged :: LineElem -> Bool
elemChanged ((_, ss), _)
    =  Atom StyleAdded   `Set.member` ss
    || Atom StyleDeleted `Set.member` ss
    || Atom StyleChanged `Set.member` ss

-- | TUNING: unify with Draft.getSelectors
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

-- | Take a document in diff mode, a number of blocks to keep preceeding and succeeding each changed
-- block, resp., and returns with all sequences of blocks far away from any change replaced by a
-- single block reading `...`.
hideUnchangedParts :: RawContent -> Int -> Int -> RawContent
hideUnchangedParts (NEL.toList . rawContentToDoc -> doc) blocksbefore blocksafter
    = docToRawContent
    . NEL.fromList
    . concatMap showRegion
    . groupBy ((==) `on` fst)
    $ zip displayedLines doc
  where
    showRegion bs@((True, _): _) = snd <$> bs
    showRegion ((False, blockKeyFromDocBlock -> bk): _) = [dotDotDot (bk <> BlockKey "_...")]
    showRegion _ = error "impossible"

    blockKeyFromDocBlock = unNonEditable . snd . fst :: DocBlock -> BlockKey

    dotDotDot bk = DocBlock NormalText (BlockDepth 0) bk [((Atom Nothing, mempty), "...")]

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
