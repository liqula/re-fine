{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.OT where

import           Data.List (groupBy, inits, tails)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as ST

import           Refine.Common.Prelude
import           Refine.Common.OT
import           Refine.Common.Types.Core hiding (Edit)

pattern DocBlock' :: Atom BlockType -> Atom BlockDepth -> NonEditable (Maybe BlockKey) -> [LineElem] -> DocBlock
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
            (patchLineElems [e | EditSecond es' <- es, e <- es'] elems)
      where
        bdpatch = [e | EditFirst es' <- es, EditFirst es'' <- es', e <- es'']
        bpatch = [e | EditFirst  es' <- bdpatch, e <- es']
        dpatch = [e | EditSecond es' <- bdpatch, e <- es']

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
        = patchLineElemText [e | EditSecond es' <- es, e <- es']
        . patchLineElemEntityStyle [e | EditFirst es' <- es, e <- es']

    patchLineElemEntityStyle :: Edit EntityStyles -> LineElem -> LineElem
    patchLineElemEntityStyle es
        = patchLineElemEntity [e | EditFirst  es' <- es, e <- es']
        . patchLineElemStyle  [e | EditSecond es' <- es, e <- es']

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


-- Auxiliary types used only in docEditRanges
type SelRange = (SelPoint, SelPoint)
type SelPoint = ((Int, Maybe BlockKey), (Int, OffsetPos))
type OffsetPos = (Bool{-begin-}, Bool{-end-})

docEditRanges :: Edit RawContent -> RawContent -> [SelectionState]
docEditRanges edits
    = concatMap toSelState
    . foldr addRange []
    . concat . zipWith blockRanges [(0::Int)..]
    . NEL.toList . showEditAsDoc (concat (coerce edits :: [Edit OTDoc])) . rawContentToDoc
  where
    blockRanges :: Int -> DocBlock -> [SelRange]
    blockRanges col (DocBlock _ _ key elems) = concat $ zipWith3 elemRanges offs' (tail offs') elems
      where
        offs :: [Int]
        offs = scanl (+) 0 $ elemLength <$> elems

        offs' :: [(Int, OffsetPos)]
        offs' = zip offs . zip (True: repeat False) $ replicate (length offs - 1) False <> [True]

        elemRanges :: (Int, OffsetPos) -> (Int, OffsetPos) -> LineElem -> [SelRange]
        elemRanges beg end ((_, ss), _) =
            [ (((col, key), beg), ((col, key), end))
            |  Atom StyleAdded   `Set.member` ss
            || Atom StyleDeleted `Set.member` ss
            || Atom StyleChanged `Set.member` ss
            ]

    elemLength :: LineElem -> Int
    elemLength ((_, ss), NonEmptyST txt)
        | Atom StyleAdded `Set.member` ss = 0
        | otherwise = ST.length txt

    addRange :: SelRange -> [SelRange] -> [SelRange]
    addRange x [] = [x]
    addRange r@(beg, e) rs@((b, end): rs')
        | e `nextTo` b = (beg, end): rs'
        | otherwise = r: rs

    nextTo :: SelPoint -> SelPoint -> Bool
    a `nextTo` b | a == b = True
    ((i, _), (_, (_, True))) `nextTo` ((j, _), (_, (True, _))) | j == i+1 = True
    _ `nextTo` _ = False

    toSelState :: SelRange -> [SelectionState]
    toSelState (((_, Just k1), (o1, _)), ((_, Just k2), (o2, _)))
        = [SelectionState False (SelectionPoint k1 o1) (SelectionPoint k2 o2)]
    toSelState _ = []


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
    showRegion ((False, _): _) = [dotDotDot]
    showRegion _ = error "impossible"

    dotDotDot = DocBlock NormalText (BlockDepth 0) Nothing [((Atom Nothing, mempty), "...")]

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

    elemChanged :: LineElem -> Bool
    elemChanged ((_, ss), _)
        =  Atom StyleAdded   `Set.member` ss
        || Atom StyleDeleted `Set.member` ss
        || Atom StyleChanged `Set.member` ss
