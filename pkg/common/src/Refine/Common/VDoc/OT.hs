{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.OT where

import           Data.List (groupBy, inits, tails)
import           Data.Foldable (toList)
import           Data.FingerTree
import           Data.Monoid.Cut
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as ST
import qualified Data.Algorithm.Patience as Diff
import           Control.Lens (ALens', mapped, cloneLens)

import           Refine.Common.Prelude
import           Refine.Common.OT
import           Refine.Common.Types.Core hiding (Edit)

pattern DocBlock' :: Atom BlockType -> Atom BlockDepth -> NonEditable BlockKey -> [LineElem] -> DocBlock
pattern DocBlock' t d k ls = (((t, d), k), Segments ls)

type Styles = Set (Atom Style)

showEditAsRawContent :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContent edits
    = docToRawContent . showEditAsDoc (concat (coerce edits :: [Edit OTDoc])) . rawContentToDoc

showEditAsDoc :: Edit OTDoc -> OTDoc -> OTDoc
showEditAsDoc edit doc
  = rawContentToDoc . toRawContent . decorateEdit
  $ fromEditRawContent (docToRawContent doc) (coerce [edit])

{-
  Adding style patches to a sequence of patches needs repeting rebase:

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

  Patch normalization:

      Delete kills insert:

        i0'c', d0, i0'd'  -->  i0'd'

      Reordering of patches:

        i2'c', i1'd'    -->  i1'd', i3'c'

  Alternative patch representation which do not need normalization:

        i2'c', d0, i3'x'   -->  del: copy: ins c: copy: ins x


-}

type Seq = FingerTree


--fmap f = mconcat . fmap (pure . f) . toList
--instance Applicative FingerTree
--instance Monad FingerTree

filterMap :: Measured b => (a -> Maybe b) -> Seq a -> Seq b
filterMap f = mconcat . map singleton . catMaybes . map f . toList

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
editStart = filterMap $ \case
  EPCopy   a -> Just a
  EPDelete a -> Just a
  EPInsert _ -> Nothing

editEnd :: Measured a => SeqEdit a -> Seq a
editEnd = filterMap $ \case
  EPCopy   a -> Just a
  EPDelete _ -> Nothing
  EPInsert a -> Just a

reverseEdit :: Measured a => SeqEdit a -> SeqEdit a
reverseEdit = fmap' $ \case
  EPCopy   a -> EPCopy   a
  EPDelete a -> EPInsert a
  EPInsert a -> EPDelete a

--composeEdits :: Measured a => SeqEdit a -> SeqEdit a -> SeqEdit a
--composeEdits = error "TODO"

--compressEdit :: SeqEdit a -> SeqEdit (Seq a)
--compressEdit = _

--expandEdit :: SeqEdit (Seq a) -> SeqEdit a
--expandEdit = _


newtype ChangeSet a = ChangeSet {unChangeSet :: Set a}
  deriving (Eq, Ord, Show)

instance Ord a => Monoid (ChangeSet a) where
  mempty = ChangeSet mempty
  ChangeSet a `mappend` ChangeSet b = ChangeSet $ Set.difference a b <> Set.difference b a

singletonChange :: a -> ChangeSet a
singletonChange = ChangeSet . Set.singleton


data DocPiece_ a
  = DocPiece (ChangeSet EStyle){-set of style *changes*-} a    -- _____s______s______
  | NewBlock BlockKey BlockDepth BlockType              -- like a newline
  deriving (Eq, Ord, Show)

type EStyle = Either Entity Style

instance Measured (DocPiece_ a) where
  type Measure (DocPiece_ a) = DocPieceMonoid
  measure = \case
    DocPiece s _     -> ((mempty,                   Uncut $ Sum 1), s)
    NewBlock key _ _ -> (((Sum 1, Last $ Just key), cut),           mempty)

type DocPieceMonoid    = (PositionMonoid, StyleMonoid)
type StyleMonoid       = ChangeSet EStyle
type PositionMonoid    = (RowIndexMonoid, ColumnIndexMonoid)
type RowIndexMonoid    = (Sum Int, Last BlockKey)
type ColumnIndexMonoid = Cut (Sum Int)

docPieceMonoidPosition :: DocPieceMonoid -> Position
docPieceMonoidPosition (((r, k), c), _) = Position (BlockIndex (getSum r - 1) (g k)) (getCol c)
  where
    g = fromMaybe (BlockKey "") . getLast

getCol :: ColumnIndexMonoid -> Int
getCol (Uncut x) = getSum x
getCol (_ :||: x) = getSum x

remaining :: ColumnIndexMonoid -> Int
remaining (Uncut x) = getSum x
remaining (x :||: _) = getSum x

type DocPiece = DocPiece_ Char -- TUNING: NonEmptyST

type NewDoc = Seq DocPiece

isValidNewDoc :: NewDoc -> Bool
isValidNewDoc d
  =  Set.null (unChangeSet sty)   -- every style is closed
  && isJust (getLast key)         -- there is at least one block in the document
  where
    (((_row, key), _col), sty) = measure d

positionPredicate :: Position -> DocPieceMonoid -> Bool
positionPredicate p = (p >=) . docPieceMonoidPosition

splitAtPosition :: Position -> NewDoc -> (NewDoc, NewDoc)
splitAtPosition = split . positionPredicate

lastPosition :: NewDoc -> Position
lastPosition = docPieceMonoidPosition . measure

type NewDocEdit = SeqEdit DocPiece

type DocEditPiece = EditPiece DocPiece

splitAtOldPosition :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtOldPosition p = split (positionPredicate p . fst)

splitAtNewPosition :: Position -> NewDocEdit -> (NewDocEdit, NewDocEdit)
splitAtNewPosition p = split (positionPredicate p . snd)

lastOldPosition :: NewDocEdit -> Position
lastOldPosition = docPieceMonoidPosition . fst . measure

lastNewPosition :: NewDocEdit -> Position
lastNewPosition = docPieceMonoidPosition . snd . measure

--concatMapEdit :: (DocEditPiece -> NewDocEdit) -> NewDocEdit -> NewDocEdit
--concatMapEdit = flip (>>=)

transformRange' :: NewDocEdit -> Range Position -> Range Position
transformRange' edit (Range p1 p2) = Range (lastNewPosition edit1) (lastNewPosition edit2)
  where
    (edit2, _) = splitAtOldPosition p2 edit
    (edit1, _) = splitAtOldPosition p1 edit2

-- TUNING
-- TODO: highlight style and block changes better
decorateEdit :: NewDocEdit -> NewDoc
decorateEdit = f mempty . map (fmap (mconcat . map singleton) . g) . groupBy ((==) `on` changeType) . toList
  where
  changeType :: EditPiece a -> Int
  changeType = \case
    EPDelete{} -> -1
    EPCopy{} -> 0
    EPInsert{} -> 1

  g :: [EditPiece a] -> EditPiece [a]
  g xs = case xs of
    EPDelete{}: _ -> EPDelete [x | EPDelete x <- xs]
    EPCopy{}: _ -> EPCopy [x | EPCopy x <- xs]
    EPInsert{}: _ -> EPInsert [x | EPInsert x <- xs]

  f :: NewDoc -> [EditPiece NewDoc] -> NewDoc
  f d = \case
    [] -> d
    EPCopy   a: es -> f (removeStyle (Set.fromList [Right StyleDeleted, Right StyleAdded]) d a) es
    EPDelete a: es -> f (addStyle (Set.singleton $ Right StyleDeleted) d a) es
    EPInsert a: es -> f (addStyle (Set.singleton $ Right StyleAdded) d a) es

addStyle :: Set EStyle -> NewDoc -> NewDoc -> NewDoc
addStyle sty d d'
    | Set.null sty' = d <> d'
    | otherwise = d <> toggleStyle (ChangeSet sty') d'
  where
    (_, ChangeSet s) = measure d
    sty' = Set.difference s sty

removeStyle :: Set EStyle -> NewDoc -> NewDoc -> NewDoc
removeStyle sty d d'
    | Set.null sty' = d <> d'
    | otherwise = d <> toggleStyle (ChangeSet sty') d'
  where
    (_, ChangeSet s) = measure d
    sty' = Set.difference sty s

toggleStyle :: ChangeSet EStyle -> NewDoc -> NewDoc
toggleStyle sty d = case split (\((_, c), _) -> remaining c >= 1) d of
  (d1, viewl -> DocPiece s c :< d) -> d1 <> (DocPiece (sty <> s) c <| d)
  _ -> d

-------- interface with current design

fromRawContent :: RawContent -> NewDoc
fromRawContent (RawContent blocks entitymap) = mconcat{-TODO-} . map fromBlock $ NEL.toList blocks
  where
    fromBlock :: Block EntityKey BlockKey -> NewDoc
    fromBlock (Block' txt entities styles btype depth key)
      = NewBlock key (BlockDepth depth) btype
      <| flip (foldr $ uncurry addStyle)
           (    (second Right <$> Set.toList styles)
             <> [ (r, Left (entitymap IntMap.! _unEntityKey ek))
                | (ek, r) <- Set.toList entities]
           )
           (mconcat . map (singleton . DocPiece mempty) $ cs txt)

    splitCol :: Int -> NewDoc -> (NewDoc, NewDoc)
    splitCol i = split $ \((_, c), _) -> getCol c >= i

    flipStyle :: EStyle -> NewDoc -> NewDoc
    flipStyle sty (viewl -> DocPiece s c :< d) = DocPiece (singletonChange sty <> s) c <| d

    addStyle :: Range Int -> EStyle -> NewDoc -> NewDoc
    addStyle (Range x y) sty (splitCol y -> (splitCol x -> (d1, d2), d3))
      = d1 <> flipStyle sty d2 <> flipStyle sty d3

toRawContent :: NewDoc -> RawContent
toRawContent = mkRawContentInternal . NEL.fromList . map toBlock . docLines
  where
    docLines :: NewDoc -> [NewDoc]
    docLines d = case split (\(((c, _), _), _) -> c >= 1) d of
      (toList -> [h], d') -> f h d'
      where
        f h d' = case split (\(((c, _), _), _) -> c >= 1) d' of
          (viewr -> line :> h', d'') -> (h <| line): f h' d''
          (viewr -> EmptyR , line) -> [h <| line]

    toBlock :: NewDoc -> Block Entity BlockKey
    toBlock (toList -> NewBlock key (BlockDepth depth) btype: d)
      = Block'
          (cs [c | DocPiece _ c <- d])
          (Set.fromList [(e, r) | (r, Left e) <- ss])
          (Set.fromList [(r, s) | (r, Right s) <- ss])
          btype
          depth
          key
      where
        ss = mkRanges mempty $ zip [0..] [s | DocPiece (ChangeSet s) _ <- d]
        mkRanges [] [] = []
        mkRanges acc ((i, s): xs) =
              [(Range beg i, sty) | (beg, sty) <- acc, sty `Set.member` s]
           <> mkRanges  (  [(beg, sty) | (beg, sty) <- acc, sty `Set.notMember` s]
                        <> [(i, sty) | sty <- Set.elems s, sty `notElem` fmap snd acc])
                        xs

-- this will not be needed in future when OTDoc is replaced by NewDoc
fromEditRawContent :: RawContent -> Edit RawContent -> NewDocEdit
fromEditRawContent rc edit
  = f $ Diff.diff (toList $ fromRawContent rc) (toList . fromRawContent $ patch edit rc)
  where
    f :: [Diff.Item DocPiece] -> NewDocEdit
    f (Diff.Both _ c: es) = EPCopy c <| f es
    f (Diff.New c: es) = EPInsert c <| f es
    f (Diff.Old c: es) = EPDelete c <| f es
    f [] = mempty

transformRange :: Edit OTDoc -> OTDoc -> Range Position -> Range Position
transformRange edits doc
    = patchFold patchBlocks (NEL.toList doc) (coerce edits)
  where
    patchFold :: Editable d => (d -> a -> EEdit d -> a) -> d -> Edit d -> a -> a
    patchFold f d es a = snd $ foldl (\(d', a') e -> (ePatch e d', f d' a' e)) (d, a) es

    patchBlocks :: [DocBlock] -> Range Position -> EEdit [DocBlock] -> Range Position
    patchBlocks bs r@(Range x y) = \case
        e@(DeleteRange i l) -> Range (delIdx' True bs bs' i l x) (delIdx' False bs bs' i l y)
          where bs' = ePatch e bs
        InsertItem i _ -> r & mapped . rowIndex %~ incIdx True i 1
        EditItem i es -> case bs !! i of
          (_, elems) -> patchFold patchLineElems elems [e | EditSecond e <- es] r

    patchLineElems :: LineElems -> Range Position -> EEdit LineElems -> Range Position
    patchLineElems (Segments (map (cs . unNonEmptyST . snd) -> bs)) r = \case
      SegmentListEdit (DeleteRange i l) ->
        r & mapped . columnIndex %~ delIdx (length . concat $ take i bs) (length . concat . take l $ drop i bs)
      SegmentListEdit (InsertItem i x) ->
        incRange columnIndex (length . concat $ take i bs) (len x) r
      SegmentListEdit (EditItem i es) ->
        patchFold (patchText . sum $ length <$> take i bs) (bs !! i) [coerce e | EditSecond e <- es] r
      JoinItems{} -> r
      SplitItem{} -> r

    len = ST.length . unNonEmptyST . snd

    patchText :: Int -> String -> Range Position -> EEdit String -> Range Position
    patchText offs _ r = \case
        DeleteRange i l -> r & mapped . columnIndex %~ delIdx (i + offs) l
        InsertItem i _ -> incRange columnIndex (i + offs) 1 r
        EditItem{} -> r

    {-
    document with selection: __###__
    if we insert a char at position 2:   ___###__  (or maybe  __####__)
    if we insert a char at position 5:   __###___  (or maybe  __####__)

    document with selection: __(___\n)____
    if we insert a line __ after the first one:  __(___\n__\n)____   or maybe: __(___\n__)\n____

    document with selection: __(___)\n____
    if we insert a line __ after the first one:  __(___)\n__\n____
    -}
    incIdx :: Bool -> Int -> Int -> Int -> Int
    incIdx isbegin i k r
      | r > i || r == i && isbegin = r + k
      | otherwise = r

    incRange :: ALens' Position Int -> Int -> Int -> Range Position -> Range Position
    incRange le i k (Range x y)
      | x' > y'   = Range y' y'
      | otherwise = Range x' y'
      where
        x' = x & cloneLens le %~ incIdx True i k
        y' = y & cloneLens le %~ incIdx False i k

    delIdx :: Int -> Int -> Int -> Int
    delIdx i k r
      | r <= i     = r
      | i + k <= r = r - k
      | otherwise  = i

    {-
    document: _
    selection: #
    ___
    __##
    ##
    ###___   -- if this is deleted, the end positon moves to the end of the previous line


    ___####____   -- if this is deleted, the end positon moves to the end of the previous line, but it does not exists, so it has to go to (0,0)
    -}
    delIdx' :: Bool -> [DocBlock] -> [DocBlock] -> Int -> Int -> Position -> Position
    delIdx' isbegin _before after i k (Position (BlockIndex r _) c) = Position (mkBI r') c'
      where
        r1 = delIdx i k r
        r2 = delIdx i k (r+1)
        (r', c')
           | r2 == r1 + 1 = (r1, c)
           | r2 == r1 && isbegin = (r1, 0)
           | r2 == r1 && not isbegin = endPos $ r1 - 1
        endPos x
          | x < 0 = (0, 0)  -- see the documentation at delIdx'
          | otherwise = (,) x $ case after !! x of DocBlock _ _ _ txt -> sum $ len <$> txt
        mkBI x = BlockIndex x $ case after !! x of DocBlock _ _ key _ -> key


docEditRanges :: Edit RawContent -> RawContent -> Ranges Position
docEditRanges edits rc
    = mconcat
    . map snd
    . docRanges True elemLength (\x -> [() | elemChanged x])
    . docToRawContent
    . showEditAsDoc (concat (coerce edits :: [Edit OTDoc]))
    $ rawContentToDoc rc
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
