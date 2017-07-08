{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Refine.Common.VDoc.OT where

import           Data.List
import           Data.Maybe
import           Data.Foldable
import           Data.FingerTree
import qualified Data.FingerTree as FingerTree
import qualified Data.Monoid.Split as Split
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as ST
import qualified Data.Algorithm.Patience as Diff

import           Refine.Common.Prelude hiding (fromList)
import           Refine.Common.OT
import           Refine.Common.Types.Core hiding (Edit)


showEditAsRawContent :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContent edit doc
  = toRawContent . decorateEdit $ fromEditRawContent doc edit

{-
  Old design: Adding style patches to a sequence of patches needs repeting rebase:

     1      2      3
  * ---> * ---> * ---> *
         |
      1s |
         v  2'     3'
         * ---> * ---> *
                |
            2's |
                v  3''
                * ---> *
                       |
                  3''s |
                       v
                       *

  This is quadratic, but could be speeded up by patch normalization:

      Delete kills insert:

        i0'c', d0, i0'd'  -->  i0'd'

      Reordering of patches:

        i2'c', i1'd'    -->  i1'd', i3'c'

  Instead of this, we use a patch representation which do not need normalization:

        i2'c', d0, i3'x'   -->  del: copy: ins c: copy: ins x

-}

type Seq = FingerTree

mapMaybeSeq :: Measured b => (a -> Maybe b) -> Seq a -> Seq b
mapMaybeSeq f = fromList . mapMaybe f . toList


data EditPiece a
  = EPCopy   a
  | EPDelete a
  | EPInsert a
  deriving (Eq, Ord, Show, Functor)

instance Measured a => Measured (EditPiece a) where
  type Measure (EditPiece a) = (Measure a, Measure a)
  measure = \case
    EPCopy a   -> (x, x) where x = measure a
    EPDelete a -> (measure a, mempty)
    EPInsert a -> (mempty, measure a)

type SeqEdit a = Seq (EditPiece a)

idEdit :: Measured a => Seq a -> SeqEdit a
idEdit = fmap' EPCopy

editStart :: Measured a => SeqEdit a -> Seq a
editStart = mapMaybeSeq $ \case
  EPCopy   a -> Just a
  EPDelete a -> Just a
  EPInsert _ -> Nothing

editEnd :: Measured a => SeqEdit a -> Seq a
editEnd = mapMaybeSeq $ \case
  EPCopy   a -> Just a
  EPDelete _ -> Nothing
  EPInsert a -> Just a

reverseEdit :: Measured a => SeqEdit a -> SeqEdit a
reverseEdit = fmap' $ \case
  EPCopy   a -> EPCopy   a
  EPDelete a -> EPInsert a
  EPInsert a -> EPDelete a

fullEdit :: Measured a => SeqEdit a -> SeqEdit a
fullEdit = fmap' $ \case
  EPCopy   a -> EPCopy   a
  EPDelete a -> EPCopy   a   -- deletion is kept
  EPInsert a -> EPInsert a

compressEdit' :: Measured a => SeqEdit a -> SeqEdit (Seq a)
compressEdit' = fromList . map (fmap fromList . g) . groupBy ((==) `on` changeType) . toList
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


newtype ChangeSet a = ChangeSet {unChangeSet :: Set a}
  deriving (Eq, Ord, Show)

instance Ord a => Monoid (ChangeSet a) where
  mempty = ChangeSet mempty
  ChangeSet a `mappend` ChangeSet b = ChangeSet $ Set.difference a b <> Set.difference b a

singletonChange :: a -> ChangeSet a
singletonChange = ChangeSet . Set.singleton


data DocPiece_ a
  = DocPiece (ChangeSet EStyle){-set of style *changes*-} a    -- _____s______s______
  | NewBlock BlockKey BlockDepth BlockType                     -- similar to a newline but with more info
  deriving (Eq, Ord, Show)

type EStyle = Either Entity Style

instance Measured Char where
  type Measure Char = Sum Int
  measure _ = Sum 1

instance (Measured a, Measure a ~ Sum Int) => Measured (DocPiece_ a) where
  type Measure (DocPiece_ a) = DocMeasure
  measure = \case
    DocPiece s c     -> DocMeasure mempty mempty (Split.M $ measure c) s
    NewBlock key _ _ -> DocMeasure (Sum 1) (Last $ Just key) Split.split mempty

data DocMeasure = DocMeasure
  { rowMeasure   :: Sum Int                -- row index + 1
  , keyMeasure   :: Last BlockKey          -- last blockkey
  , charMeasure  :: Split.Split (Sum Int)  -- column index + sum of non-newline chars before the last newline
  , styleMeasure :: ChangeSet EStyle       -- toggled styles
  }

instance Monoid DocMeasure where
  mempty = DocMeasure mempty mempty mempty mempty
  DocMeasure a b c d `mappend` DocMeasure a' b' c' d' = DocMeasure (a <> a') (b <> b') (c <> c') (d <> d')

measureChars :: DocMeasure -> Int
measureChars (DocMeasure _ _ c _) = getSum (Split.unsplit c)

measurePosition :: DocMeasure -> Position
measurePosition (DocMeasure r k c _)
  = Position (BlockIndex (getSum r - 1) (fromMaybe (BlockKey "") $ getLast k)) (getSum $ getCol c)
  where
    getCol (Split.M x) = x
    getCol (_ Split.:| x) = x

type DocPiece = DocPiece_ Char -- TUNING: NonEmptyST

type NewDoc = Seq DocPiece

isValidNewDoc :: NewDoc -> Bool
isValidNewDoc (viewl -> NewBlock{} :< _) = True   -- the first element should be a NewBlock
isValidNewDoc _ = False

positionPredicate :: Position -> DocMeasure -> Bool
positionPredicate p = (> p) . measurePosition

positionPredicate' :: Position -> DocMeasure -> Bool
positionPredicate' p = (>= p) . measurePosition

splitAtPosition :: Position -> NewDoc -> (NewDoc, NewDoc)
splitAtPosition = split . positionPredicate

lastPosition :: NewDoc -> Position
lastPosition = measurePosition . measure

firstDocPiece :: NewDoc -> Maybe (NewDoc, ChangeSet EStyle, Char, NewDoc)
firstDocPiece (split ((>= 1) . measureChars) -> (d1, viewl -> DocPiece s c :< d2)) = Just (d1, s, c, d2)
firstDocPiece _ = Nothing

firstNewBlock :: NewDoc -> Maybe (NewDoc, BlockKey, BlockDepth, BlockType, NewDoc)
firstNewBlock (split ((>= 1) . (^. rowIndex) . measurePosition) -> (d1, viewl -> NewBlock k d ty :< d2)) = Just (d1, k, d, ty, d2)
firstNewBlock _ = Nothing

toggleStyle :: ChangeSet EStyle -> NewDoc -> NewDoc
toggleStyle sty d
  | Set.null $ unChangeSet sty = d  -- this line is an optimization
  | otherwise = maybe d (\(d1, s, c, d2) -> (d1 |> DocPiece (sty <> s) c) <> d2) $ firstDocPiece d

changeStyle :: (Set EStyle -> Set EStyle) -> NewDoc -> NewDoc -> NewDoc
changeStyle fun d@(measure -> DocMeasure _ _ _ (ChangeSet sty)) d' = d <> toggleStyle (ChangeSet $ fun sty) d'

addStyle :: Set EStyle -> NewDoc -> NewDoc -> NewDoc
addStyle = changeStyle . Set.difference

removeStyle :: Set EStyle -> NewDoc -> NewDoc -> NewDoc
removeStyle = changeStyle . Set.intersection

glue :: NewDoc -> NewDoc -> NewDoc
glue = changeStyle id

docLines :: NewDoc -> [NewDoc]
docLines = filter (not . FingerTree.null) . f mempty
  where
   f acc (firstNewBlock -> Just (d1, k, d, ty, d2)) = (acc <> d1): f (singleton $ NewBlock k d ty) d2
   f acc d = [acc <> d]


type NewDocEdit = SeqEdit DocPiece

type DocEditPiece = EditPiece DocPiece

splitAtOldPosition :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtOldPosition p = split (positionPredicate p . fst)

splitAtOldPosition' :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtOldPosition' p = split (positionPredicate' p . fst)

splitAtNewPosition :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtNewPosition p = split (positionPredicate p . snd)

lastOldPosition :: NewDocEdit -> Position
lastOldPosition = measurePosition . fst . measure

lastNewPosition :: NewDocEdit -> Position
lastNewPosition = measurePosition . snd . measure

transformRange_ :: NewDocEdit -> Range Position -> Range Position
transformRange_ edit (Range p1 p2)
    = range (lastNewPosition $ edit1 <> newlines) (lastNewPosition edit2)
  where
    range a b | b < a     = Range a a
              | otherwise = Range a b

    (edit1, split ((>= 1) . measureChars . snd) -> (newlines, _)) = splitAtOldPosition p1 edit
    edit2 = fst $ splitAtOldPosition p2 edit

-- FIXME: highlight style and block changes better
decorateEdit :: NewDocEdit -> NewDoc
decorateEdit = foldl f mempty . toList . compressEdit'
  where
  f :: NewDoc -> EditPiece NewDoc -> NewDoc
  f d = \case
    EPCopy   a -> removeStyle (Set.fromList [Right StyleDeleted, Right StyleAdded]) d a   -- copy
    EPDelete a -> addStyle (Set.singleton $ Right StyleDeleted) d a                       -- copy
    EPInsert a -> addStyle (Set.singleton $ Right StyleAdded) d a                         -- insert

-------- interface with current design

fromRawContent :: RawContent -> NewDoc
fromRawContent (RawContent blocks entitymap) = foldl glue mempty . map fromBlock $ NEL.toList blocks
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
    addStyle' (Range x y) (Set.singleton -> s) (splitCol y -> (splitCol x -> (d1, d2), d3))
      = removeStyle s (addStyle s d1 d2) d3

    splitCol :: Int -> NewDoc -> (NewDoc, NewDoc)
    splitCol i = split $ (> i) . (^. columnIndex) . measurePosition

toRawContent :: NewDoc -> RawContent
toRawContent (docLines -> dls) = mkRawContentInternal . NEL.fromList $ zipWith toBlock (scanl (<>) mempty $ styleMeasure . measure <$> dls) dls
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
           $ [s | DocPiece (ChangeSet s) _ <- d] <> [unChangeSet $ css <> styleMeasure (measure nd)]

        mkRanges _ [] = []
        mkRanges acc ((i, s): xs) =
              [(Range beg i, sty) | (beg, sty) <- acc, sty `Set.member` s, i > beg]
           <> mkRanges  (  [(beg, sty) | (beg, sty) <- acc, sty `Set.notMember` s]
                        <> [(i, sty) | sty <- Set.elems s, sty `notElem` fmap snd acc])
                        xs

-- temporary hack
-- this will not be needed in future when OTDoc is replaced by NewDoc
fromEditRawContent :: RawContent -> Edit RawContent -> NewDocEdit
fromEditRawContent rc edit
  = fromList . map f $ Diff.diff (toList $ fromRawContent rc) (toList . fromRawContent $ patch edit rc)
  where
    f (Diff.Both c _) = EPCopy   c
    f (Diff.New  c  ) = EPInsert c
    f (Diff.Old  c  ) = EPDelete c

-- FIXME: use RawContent instead of OTDoc
transformRange :: Edit OTDoc -> OTDoc -> Range Position -> Range Position
transformRange edit doc = transformRange_ $ fromEditRawContent (docToRawContent doc) (coerce [edit])

transformEditRange :: RawContent -> Edit RawContent -> Range Position -> Range Position
transformEditRange rc edit = transformRange_ . fullEdit $ fromEditRawContent rc edit

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

-- Refactoring: unify with Draft.getSelectors
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
