{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.OT where

import           Data.List (groupBy, inits, tails)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as ST
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
showEditAsDoc edits
    = NEL.fromList . patchBlocks (coerce edits) . NEL.toList
  where
    -- TUNING: compress/rearrange the elementary edits first or use a vector instead of lists?
    patchBlocks :: Edit [DocBlock] -> [DocBlock] -> [DocBlock]
    patchBlocks [] bs = bs
    patchBlocks (e:es) bs = case e of
        DeleteRange i l -> patchBlocks (incIdx i l <$> es)
                         $ take i bs <> (markBlock StyleDeleted <$> take l (drop i bs)) <> drop (i+l) bs
        InsertItem i x  -> patchBlocks es $ take i bs <> (markBlock StyleAdded x: drop i bs)
        EditItem i edit -> patchBlocks es $ take i bs <> (patchBlock (bs !! i) edit: drop (i+1) bs)

    patchBlock :: DocBlock -> Edit DocBlock -> DocBlock
    patchBlock (DocBlock' btype depth key elems) es = (if null bpatch && null dpatch then id else markBlock StyleChanged)
        $ DocBlock'
            (patch bpatch btype)
            (patch dpatch depth)
            key
            (patchLineElems [e | EditSecond e <- es] elems)
      where
        bdpatch = [e | EditFirst (EditFirst e) <- es]
        bpatch = [e | EditFirst  e <- bdpatch]
        dpatch = [e | EditSecond e <- bdpatch]

    patchLineElems :: Edit LineElems -> [LineElem] -> [LineElem]
    patchLineElems [] bs = bs
    patchLineElems (SegmentListEdit (DeleteRange i l): es) bs
        = patchLineElems (incIdx' i l <$> es) $ take i bs <> (markElem StyleDeleted <$> take l (drop i bs)) <> drop (i+l) bs
    patchLineElems (SegmentListEdit (InsertItem i x): es) bs
        = patchLineElems es $ take i bs <> (markElem StyleAdded x: drop i bs)
    patchLineElems (SegmentListEdit (EditItem i edit): es) bs
        = patchLineElems (incIdx' i (length ls - 1) <$> es) $ take i bs <> (ls <> drop (i+1) bs)
      where ls = patchLineElem edit (bs !! i)
    patchLineElems (JoinItems i: es) (splitAt i -> (as, (a1, b1): (a2, b2): bs))
        | a1 /= a2 = error "impossible: items are not joinable"
        | otherwise = patchLineElems es $ as <> (markElem StyleChanged (a1, joinItems b1 b2): bs)
    patchLineElems (SplitItem i j: es) (splitAt i -> (as, (a, splitItem j -> (b1, b2)): bs))
        = patchLineElems es $ as <> (markElem StyleChanged (a, b1): markElem StyleChanged (a, b2): bs)

    patchLineElem :: Edit LineElem -> LineElem -> [LineElem]
    patchLineElem es
        = patchLineElemText [e | EditSecond e <- es]
        . patchLineElemEntityStyle [e | EditFirst e <- es]

    patchLineElemEntityStyle :: Edit EntityStyles -> LineElem -> LineElem
    patchLineElemEntityStyle es
        = patchLineElemEntity [e | EditFirst  e <- es]
        . patchLineElemStyle  [e | EditSecond e <- es]

    patchLineElemEntity :: Edit (Atom (Maybe Entity)) -> LineElem -> LineElem
    patchLineElemEntity [] le = le
    patchLineElemEntity e ((ent, ss), text) = markElem StyleChanged ((patch e ent, ss), text)

    patchLineElemStyle :: Edit (Set (Atom Style)) -> LineElem -> LineElem
    patchLineElemStyle [] le = le
    patchLineElemStyle e ((ent, ss), text) = markElem StyleChanged ((ent, patch e ss), text)

    patchLineElemText :: Edit NonEmptyST -> LineElem -> [LineElem]
    patchLineElemText es ((ent, as), text)
        = [ ((ent, as <> marks), text')
          | (marks, text') <- compress $ patchText ((,) noStyle <$> cs text) (coerce es)
          ]

    patchText :: [(Styles, Char)] -> Edit String -> [(Styles, Char)]
    patchText bs [] = bs
    patchText bs (e:es) = case e of
        DeleteRange i l -> patchText (take i bs <> (markStyle StyleDeleted <$> take l (drop i bs)) <> drop (i+l) bs)
                                   $ incIdx i l <$> es
        InsertItem i x  -> patchText (take i bs <> (markStyle StyleAdded (noStyle, x): drop i bs)) es
        EditItem i edit -> patchText (take i bs <> (foldl patchChar (bs !! i) edit: drop (i+1) bs)) es

    patchChar :: (Styles, Char) -> EEdit Char -> (Styles, Char)
    patchChar a _ = markStyle StyleChanged a

    compress :: [(Styles, Char)] -> [(Styles, NonEmptyST)]
    compress = fmap (\xs -> (fst (head xs), NonEmptyST $ cs (snd <$> xs))) . groupBy ((==) `on` fst)

    incIdx i k = \case
        DeleteRange j l | j >= i    -> DeleteRange (j + k) l
                        | otherwise -> DeleteRange j l
        InsertItem j x | j >= i    -> InsertItem (j + k) x
                       | otherwise -> InsertItem j x
        EditItem j x   | j >= i    -> EditItem (j + k) x
                       | otherwise -> EditItem j x

    incIdx' i k = \case
        SegmentListEdit e -> SegmentListEdit $ incIdx i k e
        JoinItems j   | j >= i    -> JoinItems $ j + k
                      | otherwise -> JoinItems j
        SplitItem j x | j >= i    -> SplitItem (j + k) x
                      | otherwise -> SplitItem j x

    markBlock sty (DocBlock btype depth key elems) = DocBlock btype depth key (markElem sty <$> elems)

    markElem :: Style -> (EntityStyles, a) -> (EntityStyles, a)
    markElem = first . second . addStyle

    markStyle :: Style -> (Styles, a) -> (Styles, a)
    markStyle = first . addStyle

    addStyle sty = (Set.singleton (Atom sty) <>)

    noStyle :: Styles
    noStyle = mempty


transformRange :: Edit OTDoc -> OTDoc -> Range Position -> Range Position
transformRange edits doc
    = patchFold patchBlocks (coerce edits) (NEL.toList doc)
  where
    patchFold :: Editable d => (d -> a -> EEdit d -> a) -> Edit d -> d -> a -> a
    patchFold f es d a = snd $ foldl (\(d', a') e -> (ePatch e d', f d' a' e)) (d, a) es

    patchBlocks :: [DocBlock] -> Range Position -> EEdit [DocBlock] -> Range Position
    patchBlocks bs r@(Range x y) = \case
        e@(DeleteRange i l) -> Range (delIdx' True bs bs' i l x) (delIdx' False bs bs' i l y)
          where bs' = ePatch e bs
        InsertItem i _ -> r & mapped . rowIndex %~ incIdx True i 1
        EditItem i es -> case bs !! i of
          (_, elems) -> patchFold patchLineElems [e | EditSecond e <- es] elems r

    patchLineElems :: LineElems -> Range Position -> EEdit LineElems -> Range Position
    patchLineElems (Segments (map (cs . unNonEmptyST . snd) -> bs)) r = \case
      SegmentListEdit (DeleteRange i l) ->
        r & mapped . columnIndex %~ delIdx (length . concat $ take i bs) (length . concat . take l $ drop i bs)
      SegmentListEdit (InsertItem i x) ->
        incRange columnIndex (length . concat $ take i bs) (len x) r
      SegmentListEdit (EditItem i es) ->
        patchFold (patchText . sum $ length <$> take i bs) [coerce e | EditSecond e <- es] (bs !! i) r
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
