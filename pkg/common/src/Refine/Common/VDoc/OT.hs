{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
  , compressEdit

  , measureStylePositions
  , measurePosition
  , isValidNewDoc

  , splitAtPosition
  , lastPosition
  , splitAtOldPosition
  , splitAtNewPosition
  , lastOldPosition
  , lastNewPosition
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
  ) where

import qualified Data.Algorithm.Patience as Diff
import           Data.FingerTree hiding (reverse, null)
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

import           Refine.Common.OT hiding (compressEdit)
import           Refine.Common.Prelude hiding (fromList)
import           Refine.Common.Types.Core hiding (Edit)


-- * fingertree amendments

-- FUTUREWORK: make this a patch to https://github.com/pavelchristof/fingertree-tf.

mapMaybeFT :: Measured b => (a -> Maybe b) -> FingerTree a -> FingerTree b
mapMaybeFT f = fromList . mapMaybe f . toList

-- | Call 'split' on 'snd' repeatedly and return the list of 'fst's.  Termination: if 'snd' is
-- empty, return empty list; if not, return at least the first 'splits' is run on its tail.
--
-- FIXME: if the boolean function on measure does not return 'True' on the first element of some
-- intermediate 'FingerTree', the output is
splits :: Measured a => (Measure a -> Bool) -> FingerTree a -> [FingerTree a]
splits p (split p -> (d1, d2)) = d1: case viewl d2 of
  EmptyL -> []
  x :< xs -> mapHead (x <|) $ splits p xs
  where
    mapHead f (x: xs) = f x: xs
    mapHead _ _ = error "impossible"

-- | List of measures for all splits (accumulates).
splitMeasures :: Measured a => (Measure a -> Bool) -> FingerTree a -> [Measure a]
splitMeasures p = scanl1 (<>) . map measure . splits p

-- | Take a @b@-valued function on measures (like for 'split') and a list of values of that
-- function, and split a 'FingerTree' up into the points where those values are reached.  Function
-- must be monotonous in @b@.
--
-- Example for @b@: 'Position'.  This function could also be used to e.g. split at all mark
-- beginnings.
splitsAt :: (Measured a, Ord b) => (Measure a -> b) -> [b] -> FingerTree a -> [FingerTree a]
splitsAt f bs_ = reverse . g (reverse bs_)
  where
  g [] d = [d]
  g (b: bs) (split ((> b) . f) -> (d1, d2)) = d2: g bs d1


-- * ChangeSet, ChangeMap

-- | Keep track of styling in 'FingerTree's.  This is associated with every element in the finger
-- tree.  Toggle @a@ in and out of the set with '(<>)'.
--
-- ChangeSets accumulate: a 'DocPiece' is styled with all styles in all ChangeSets associated with
-- 'DocPiece's to the left in this position.  (This is both more memory compact and faster: if you
-- delete a mark, you cut the doc at the pieces where the mark style changes and remove it there.
-- No other pieces of the doc need to be touched.)
newtype ChangeSet a = ChangeSet {unChangeSet :: Set a}
  deriving (Eq, Ord, Show)

instance Ord a => Monoid (ChangeSet a) where
  mempty = ChangeSet mempty
  ChangeSet a `mappend` ChangeSet b
    | Set.null a = ChangeSet b     -- optimization for better sharing
    | Set.null b = ChangeSet a     -- optimization for better sharing
    | otherwise = ChangeSet $ Set.difference a b <> Set.difference b a

-- | Like 'ChangeSet', but for 'Measure'.  Counting the number of toggles is necessary here to make
-- the functions on measures monotonic.
newtype ChangeMap a = ChangeMap {unChangeMap :: Map a Int}
  deriving (Eq, Ord, Show)

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
toChangeSet = ChangeSet . Map.keysSet . Map.filter odd . unChangeMap

-- | See 'toChangeMap'.
toChangeMap :: ChangeSet a -> ChangeMap a
toChangeMap = ChangeMap . Map.fromSet (const 1) . unChangeSet


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

type FTEdit a = FingerTree (EditPiece a)

-- | edit with same start and end object.
idEdit :: Measured a => FingerTree a -> FTEdit a
idEdit = fmap' EPCopy

-- | document before edit.  names are motivated by cat theory: this is the start of the edit arrow.
editStart :: Measured a => FTEdit a -> FingerTree a
editStart = mapMaybeFT $ \case
  EPCopy   a -> Just a
  EPDelete a -> Just a
  EPInsert _ -> Nothing

-- | document after edit.
editEnd :: Measured a => FTEdit a -> FingerTree a
editEnd = mapMaybeFT $ \case
  EPCopy   a -> Just a
  EPDelete _ -> Nothing
  EPInsert a -> Just a

reverseEdit :: Measured a => FTEdit a -> FTEdit a
reverseEdit = fmap' $ \case
  EPCopy   a -> EPCopy   a
  EPDelete a -> EPInsert a
  EPInsert a -> EPDelete a

-- | keep deleted parts of the doc.  used when transforming ranges to diff view.
fullEdit :: Measured a => FTEdit a -> FTEdit a
fullEdit = fmap' $ \case
  EPCopy   a -> EPCopy   a
  EPDelete a -> EPCopy   a
  EPInsert a -> EPInsert a

-- | Helper (for performance).  Group consecutive 'EditPiece' of the same kind (delete, copy,
-- insert) in sub-fingertrees.  This allows e.g. to style them together in a more efficient way.
-- See 'decorateEdit'.
compressEdit :: Measured a => FTEdit a -> FTEdit (FingerTree a)
compressEdit = fromList . map (fmap fromList . g) . groupBy ((==) `on` changeType) . toList
  where
  changeType :: EditPiece a -> Int
  changeType = \case
    EPDelete{} -> -1
    EPCopy{}   ->  0
    EPInsert{} ->  1

  g :: [EditPiece a] -> EditPiece [a]
  g xs = case xs of
    EPDelete{}: _ -> EPDelete [x | EPDelete x <- xs]
    EPCopy{}:   _ -> EPCopy   [x | EPCopy x   <- xs]
    EPInsert{}: _ -> EPInsert [x | EPInsert x <- xs]


-- * DocPiece

-- | TUNING: use 'NonEmptyST' (or @Either Char NonEmptyST@?  probably just 'NonEmptyST'...)  This is
-- probably important.
type DocPiece = DocPiece_ Char

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
  { rowMeasure   :: Sum Int                -- ^ row index + 1
  , keyMeasure   :: Last BlockKey          -- ^ last blockkey
  , charMeasure  :: Split.Split (Sum Int)  -- ^ column index + sum of non-newline chars before the last newline
  , styleMeasure :: ChangeMap EStyle       -- ^ styles applicable to a given doc piece
  }

instance Monoid DocMeasure where
  mempty = DocMeasure mempty mempty mempty mempty
  DocMeasure a b c d `mappend` DocMeasure a' b' c' d' = DocMeasure (a <> a') (b <> b') (c <> c') (d <> d')

-- | 'ChangeMap' from 'DocMeasure', translated into a 'ChangeSet'.
styleSet :: DocMeasure -> ChangeSet EStyle
styleSet = toChangeSet . styleMeasure

-- | Count non-newline characters.
measureStylePositions :: DocMeasure -> Int
measureStylePositions = getSum . Split.unsplit . charMeasure

measurePosition :: DocMeasure -> Position
measurePosition (DocMeasure r k c _)
  = Position (BlockIndex (getSum r - 1) (fromMaybe (BlockKey "") $ getLast k)) (getSum $ getCol c)
  where
    getCol (Split.M x) = x
    getCol (_ Split.:| x) = x


-- * NewDoc

-- | Representation of 'RawContent' better suited for dealing with 'Edit's.  Blocks are prefixed
-- with @'\n'@ and appended: @["oumpf", "", "boo"] => "\noumpf\n\nboo"@.
type NewDoc = FingerTree DocPiece

-- | Check for non-emptiness.  (Styles are not toggled off if they apply to the last char in a doc.
-- This is the natural behavior given that closing a style range is done on the first char *after*
-- the range (which does not exist in this case), and has no known drawbacks.  use 'syleSet' to
-- observe open styles at the very end of the doc.
--
-- FIXME: return 'False' if style ranges overlap.  (did we forget other validity criteria?)
isValidNewDoc :: NewDoc -> Bool
isValidNewDoc (viewl -> NewBlock{} :< _) = True
isValidNewDoc _ = False

positionPredicate :: Position -> DocMeasure -> Bool
positionPredicate p = (> p) . measurePosition

splitAtPosition :: Position -> NewDoc -> (NewDoc, NewDoc)
splitAtPosition = split . positionPredicate

lastPosition :: NewDoc -> Position
lastPosition = measurePosition . measure

toggleStyle :: ChangeSet EStyle -> NewDoc -> NewDoc
toggleStyle sty d
  | Set.null $ unChangeSet sty = d  -- this line is an optimization
  | (d1, viewl -> DocPiece s c :< d2) <- split ((>= 1) . measureStylePositions) d
      = d1 <> (DocPiece (sty <> s) c <| d2)
  | otherwise = d

mappendWithStyleChange :: (Set EStyle -> Set EStyle) -> NewDoc -> NewDoc -> NewDoc
mappendWithStyleChange fun d@(styleSet . measure -> c@(ChangeSet sty)) d' = d <> toggleStyle (c <> ChangeSet (fun sty)) d'

mappendNewDoc :: NewDoc -> NewDoc -> NewDoc
mappendNewDoc = mappendWithStyleChange $ const mempty

-- | used for e.g. filtering out all marks.
filterStyle :: (EStyle -> Bool) -> NewDoc -> NewDoc
filterStyle p = fmap' $ \case
    DocPiece (ChangeSet s) c -> DocPiece (ChangeSet $ Set.filter p s) c
    x -> x

-- | all styles occurring anywhere in a 'NewDoc'.
docStyles :: NewDoc -> Set EStyle
docStyles = Map.keysSet . Map.filter (> 0) . unChangeMap . styleMeasure . measure

-- | all ranges decorated with a given style.
getStyleRanges :: NewDoc -> EStyle -> Ranges Position
getStyleRanges doc sty
  = RangesInner [Range a b | [a, b] <- everyNth 2 $ splitPositions (Map.member sty . unChangeMap . styleMeasure) doc]
  where
    everyNth :: Int -> [a] -> [[a]]
    everyNth _ [] = []
    everyNth i xs = take i xs: everyNth i (drop i xs)

addStyleRanges :: EStyle -> Ranges Position -> NewDoc -> NewDoc
addStyleRanges s rs
  = mconcat
  . mapTail (toggleStyle . ChangeSet $ Set.singleton s)
  . splitsAt measurePosition [p | Range a b <- unRanges rs, p <- [a, b]]
  where
  mapTail f (x: xs) = x: map f xs
  mapTail _ _ = error "impossible"

-- | Like 'splitMeasures', but producing a 'Position' list.
splitPositions :: (DocMeasure -> Bool) -> NewDoc -> [Position]
splitPositions p = map measurePosition . splitMeasures p

-- | Split up a 'NewDoc' into segments corresponding to 'Block's in 'RawContent'.
-- @docBlocks "\noumph\n\n\nboo" == ["\noumph", "\n", "\n", "\nboo"]@
docBlocks :: NewDoc -> [NewDoc]
docBlocks ndoc = case splits ((>= 1) . rowMeasure) ndoc of
  (FingerTree.null -> True): v -> v
  bad -> error $ "docBlocks: invalid input: " <> show bad


-- * NewDocEdit

-- | TUNING: @newtype NewDocEdit = NewDocEdit (FTEdit NewDoc)@, then all edits would always be
-- compressed implicitly inside custom 'split' and 'mappend'.
type NewDocEdit = FTEdit DocPiece

splitAtOldPosition :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtOldPosition p = split (positionPredicate p . fst)

splitAtNewPosition :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtNewPosition p = split (positionPredicate p . snd)

lastOldPosition :: NewDocEdit -> Position
lastOldPosition = measurePosition . fst . measure

lastNewPosition :: NewDocEdit -> Position
lastNewPosition = measurePosition . snd . measure

transformRange :: NewDocEdit -> Range Position -> Range Position
transformRange edit (Range p1 p2)
    = range (lastNewPosition $ edit1 <> newlines) (lastNewPosition edit2)
  where
    range a b | b < a     = Range a a
              | otherwise = Range a b

    (edit1, split ((>= 1) . measureStylePositions . snd) -> (newlines, _)) = splitAtOldPosition p1 edit
    edit2 = fst $ splitAtOldPosition p2 edit

decorateEdit :: NewDocEdit -> NewDoc
decorateEdit = foldl f mempty . toList . compressEdit
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
      =  NewBlock key (BlockDepth depth) btype
      <| foldr (uncurry addStyle')
               (fromList . map (DocPiece mempty) $ cs txt)
               (    (second Right <$> Set.toList styles)
                 <> [ (r, Left (entitymap IntMap.! _unEntityKey ek))
                    | (ek, r) <- Set.toList entities]
               )

    addStyle' :: Range Int -> EStyle -> NewDoc -> NewDoc
    addStyle' (Range x y) s (splitCol y -> (splitCol x -> (d1, d2), d3))
      = mappendWithStyleChange (Set.delete s) (mappendWithStyleChange (Set.insert s) d1 d2) d3

    splitCol :: Int -> NewDoc -> (NewDoc, NewDoc)
    splitCol i = split $ (> i) . (^. columnIndex) . measurePosition

toRawContent :: NewDoc -> RawContent
toRawContent (docBlocks -> dls)
  = mkRawContentInternal . NEL.fromList $ zipWith toBlock (scanl (<>) mempty $ styleSet . measure <$> dls) dls
  where
    toBlock :: ChangeSet EStyle -> NewDoc -> Block Entity BlockKey
    toBlock css@(ChangeSet sty_) nd@(toList -> NewBlock key (BlockDepth depth) btype: d)
      = Block'
          (cs [c | DocPiece _ c <- d])
          (Set.fromList [(e, r) | (r, Left e) <- ss])
          (Set.fromList [(r, s) | (r, Right s) <- ss])
          btype
          depth
          key
      where
        ss = mkRanges ((,) 0 <$> Set.toList sty_) . zip [0..]
           $ [s | DocPiece (ChangeSet s) _ <- d] <> [unChangeSet $ css <> styleSet (measure nd)]

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
  = fromList . map f $ Diff.diff (toList $ fromRawContent rc) (toList . fromRawContent $ patch edit rc)
  where
    f (Diff.Both c _) = EPCopy   c
    f (Diff.New  c  ) = EPInsert c
    f (Diff.Old  c  ) = EPDelete c


-- * 'OTDoc'

transformRangeOTDoc :: Edit OTDoc -> OTDoc -> Range Position -> Range Position
transformRangeOTDoc edit doc = transformRange $ fromEditRawContent (docToRawContent doc) (coerce [edit])

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

    transformEditRange = transformRange . fullEdit $ dedit

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
